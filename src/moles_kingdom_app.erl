-module(moles_kingdom_app).

-behaviour(application).

-export([start/2, stop/1]).

-export([start/0, stop/0]).


%% ----------------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------------
start() -> {ok,_} = application:ensure_all_started(moles_kingdom).

stop() -> application:stop(moles_kingdom).


%% ----------------------------------------------------------------------------
%% application callbacks
%% ----------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
		{'_',[
			{"/websocket", ws_handler, []}
		]}
	]),
	{ok,_} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),

	%pong = 
	net_adm:ping('mole0@127.0.0.1'), %FIXME
    moles_kingdom_sup:start_link().


stop(_State) ->
    ok.
