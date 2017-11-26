rebar ?= ./rebar3
rebar_cmd = $(rebar) $(profile:%=as %)

compile:
	@$(rebar_cmd) compile

clean:
	@$(rebar_cmd) clean

start: compile
	erl -name moles_kingdom@127.0.0.1 -setcookie moles -pa _build/default/lib/*/ebin -s moles_kingdom_app

# to stop, use rpc:call('moles_kingdom@127.0.0.1', init, stop, []).
# this will stop heart program
# $erl -name node1@127.0.0.1 -setcookie moles
# $> net_adm:ping('moles_kingdom@127.0.0.1').
service: compile
	erl -name moles_kingdom@127.0.0.1 -setcookie moles -pa _build/default/lib/*/ebin -noshell -detached -s moles_kingdom_app &
