ERL_RUN_ARGS:=-pa ebin -boot start_sasl -s wgconfig_app start

compile:
	rebar compile skip_deps=true

compile-all:
	rebar compile

get-deps:
	rebar get-deps

clean:
	rebar clean skip_deps=true
	rm -f erl_crash.dump

clean-all:
	rebar clean
	rm -f erl_crash.dump

eunit:
	rebar eunit skip_deps=true

run:
	ERL_LIBS=deps erl $(ERL_RUN_ARGS)

d:
	dialyzer --src -I include src

etags:
	etags src/*
