rebar ?= ./rebar3
rebar_cmd = $(rebar) $(profile:%=as %)

compile:
	@$(rebar_cmd) compile

clean:
	@$(rebar_cmd) clean

start: compile
	erl -name moles_kingdom@127.0.0.1 -setcookie moles -pa _build/default/lib/*/ebin -s moles_kingdom_app

service: compile
	erl -name moles_kingdom@127.0.0.1 -setcookie moles -pa _build/default/lib/*/ebin -noshell -detached -s moles_kingdom_app &
