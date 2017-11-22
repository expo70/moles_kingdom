-module(ws_handler).

%% cowboy_websocket callbacks
-export([
	init/2, websocket_init/1, websocket_handle/2,
	websocket_info/2, terminate/3
]).


-record(state,
	{
		peer_name,
		handler,
		tid_subs
	}).


%% ----------------------------------------------------------------------------
%% cowboy_websocket callbacks
%% ----------------------------------------------------------------------------
% NOTE: init() is not excecuted in the handler process
init(Req, _State) ->
	{Host, Port} = cowboy_req:peer(Req),
	PeerName = list_to_binary(string:join([inet_parse:ntoa(Host),
		":", io_lib:format("~p", [Port])], "")),
	
	{cowboy_websocket, Req, #state{peer_name=PeerName}}.


websocket_init(State) ->
	Handler = ebus_proc:spawn_handler(fun handle_msg/2, [self()]),
	TidSubs = ets:new(subs, []),

	{ok, State#state{handler=Handler, tid_subs=TidSubs}}.


%% binary text received through websocket
%%
%% JSON inputs:
%% {"cmd" : "sub",   "coord" : [x,y]}
%% {"cmd" : "unsub", "coord" : [x,y]}
websocket_handle({text, CmdJSON}, State) ->
	Tid = State#state.tid_subs,
	#{<<"cmd">> := Cmd, <<"coord">> := Coord} = jsone:decode(CmdJSON),

	Msg =
	case Cmd of
		<<"sub">> ->
			% we do not detect duplicated subs
			true = ets:insert(Tid, {Coord}),
			io:format("try to subscribe topic: ~p~n", [Coord]),
			case ebus:sub(State#state.handler, Coord) of
				ok -> #{<<"error">> => <<"no_error">>};
				{error, Reason} -> #{<<"error">> => Reason}
			end;
		<<"unsub">> ->
			% we do not detect unsubs of subscribed topics
			true = ets:delete(Tid, {Coord}),
			io:format("try to unsubscribe topic: ~p~n", [Coord]),
			case ebus:unsub(State#state.handler, Coord) of
				ok -> #{<<"error">> => <<"no_error">>};
				{error, Reason} -> #{<<"error">> => Reason}
			end
	end,

	MsgJSON = jsone:encode(Msg),

	{reply, {text, MsgJSON}, State};

websocket_handle(_InFrame, State) ->
	{ok, State}.


websocket_info({message_published, What}, State) ->
	MsgJSON = jsone:encode(What),

	{reply, {text, MsgJSON}, State};

websocket_info(_Info, State) ->
	{ok, State}.


terminate(Reason, _PartialReq, State) ->
	io:format("ws_handler:terminate, Reason=~p~n",[Reason]),
	
	case State#state.tid_subs of
		undefined -> ok;
		Tid ->
			ets:foldl(fun({C},AccIn)-> [ebus:unsub(State#state.handler, C)|
				AccIn] end, [], Tid)
	end,
	ok.


%% called in the spawned process
handle_msg(What, Pid) ->
	Pid ! {message_published, What}.



