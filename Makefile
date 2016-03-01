compile:
	rebar3 compile

clean:
	rebar3 clean
	rm -f erl_crash.dump

tests:
	rebar3 eunit
	rebar3 ct

run:
	erl -pa _build/default/lib/*/ebin -config sys -s wgconfig_app start
