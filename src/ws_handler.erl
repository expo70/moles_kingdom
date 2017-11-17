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
init(Req, State) ->
	{{Host, Port}, _} = cowboy_req:peer(Req),
	PeerName = list_to_binary(string:join([inet_parse:ntoa(Host),
		":", io_lib:format("~p", [Port])], "")),
	
	{cowboy_websocket, Req, State#state{peer_name=PeerName}}.


websocket_init(State) ->
	Handler = ebus_proc:spawn_handler(fun handle_msg/2, [self()]),
	TidSubs = ets:new(subs, []),

	{ok, State#state{handler=Handler, tid_subs=TidSubs}}.


%% binary text received through websocket
websocket_handle({text, CmdJSON}, State) ->
	Tid = State#state.tid_subs,
	Cmd = jsone:decode(CmdJSON),

	Msg =
	case Cmd of
		{sub,   Coordinate} ->
			true = ets:insert(Tid, Coordinate),
			{ebus:sub(State#state.handler, Coordinate), Coordinate};
		{unsub, Coordinate} ->
			true = ets:delete(Tid, Coordinate),
			{ebus:unsub(State#state.handler, Coordinate), Coordinate};
		_Unknown -> {error, unknown_command}
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


terminate(_Reason, _PartialReq, State) ->
	Tid = State#state.tid_subs,

	ets:foldl(fun(C,AccIn)-> [ebus:unsub(State#state.handler, C)|AccIn] end,
		[], Tid),
	ok.


%% called in the spawned process
handle_msg(What, Pid) ->
	Pid ! {message_published, What}.
