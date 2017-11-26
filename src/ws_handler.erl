-module(ws_handler).

%% cowboy_websocket callbacks
-export([
	init/2, websocket_init/1, websocket_handle/2,
	websocket_info/2, terminate/3
]).


-record(state,
	{
		peer_name,
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
	TidSubs = ets:new(subs, []),

	{ok, State#state{tid_subs=TidSubs}}.


%% Current lmitation:
%%
%% web client <-> ws_handler <-> ErlBus <- moles
%%          websocket         sub      pub
%%
%% Status updates are always pushed by moles. Web client does not obtain initial
%% states when it connect to the ws_handler because there is no way for the
%% client to tell the moles to push the status at that time; The client must
%% wait for the next update.


%% binary text received through websocket
%%
%% JSON inputs:
%% {"cmd" : "sub",   "coord" : [x,y]}
%% {"cmd" : "unsub", "coord" : [x,y]}
websocket_handle({text, CmdJSON}, State) ->
	Tid = State#state.tid_subs,
	Map = jsone:decode(CmdJSON),

	Msg =
	case maps:get(<<"cmd">>, Map) of
		<<"heartbeat">> ->
			#{<<"error">> => <<"no_error">>};
		<<"sub">> ->
			% we do not detect duplicated subs
			Coord = maps:get(<<"coord">>, Map),
			io:format("try to subscribe topic: ~p~n", [Coord]),
			% in ebus_proc:spawn_handler(Fun, Args, Opts),
			% call back fun is invoked as
			% apply(erlang,apply,[Fun, [Message|Args]])
			Handler = ebus_proc:spawn_handler(fun handle_msg/3,
				[Coord, self()]),
			true = ets:insert(Tid, {Coord, Handler}),
			case ebus:sub(Handler, Coord) of
				ok -> #{<<"error">> => <<"no_error">>};
				{error, Reason} -> #{<<"error">> => Reason}
			end;
		<<"unsub">> ->
			% we do not detect unsubs of subscribed topics
			Coord = maps:get(<<"coord">>, Map),
			io:format("try to unsubscribe topic: ~p~n", [Coord]),
			{Coord, Handler} = ets:lookup(Tid, Coord),
			true = ets:delete(Tid, Coord),
			case ebus:unsub(Handler, Coord) of
				ok -> #{<<"error">> => <<"no_error">>};
				{error, Reason} -> #{<<"error">> => Reason}
			end
	end,

	MsgJSON = jsone:encode(Msg),

	{reply, {text, MsgJSON}, State};

websocket_handle(_InFrame, State) ->
	{ok, State}.


websocket_info({message_published, What, _Coord}, State) ->
	MsgJSON = jsone:encode(What),

	{reply, {text, MsgJSON}, State};

websocket_info(_Info, State) ->
	{ok, State}.


terminate(Reason, _PartialReq, State) ->
	io:format("ws_handler:terminate, Reason=~p~n",[Reason]),
	
	case State#state.tid_subs of
		undefined -> ok;
		Tid ->
			ets:foldl(fun({C,H},AccIn)-> [ebus:unsub(H, C)|
				AccIn] end, [], Tid)
	end,
	ok.


%% called in the spawned process
handle_msg(What, Coord, Pid) ->
	Pid ! {message_published, What, Coord}.



