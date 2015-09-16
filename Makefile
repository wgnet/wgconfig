compile:
	rebar compile

clean:
	rebar clean
	rm -f erl_crash.dump

tests:
	rebar compile skip_deps=true
	rebar eunit skip_deps=true
	rebar ct skip_deps=true

run:
	erl -pa ebin -config sys -s wgconfig_app start

d:
	dialyzer --src -I include src
