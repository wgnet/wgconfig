compile:
	rebar3 compile

clean:
	-rm -rf _build
	-rm -f erl_crash.dump

eunit:
	rebar3 eunit

ct:
	rebar3 ct

run:
	erl -pa _build/default/lib/*/ebin -config sys -s wgconfig_app start
