compile:
	rebar compile

clean:
	rebar clean
	rm -f erl_crash.dump

eunit:
	rebar eunit

run:
	erl -pa ebin -config sys -s wgconfig_app start

d:
	dialyzer --src -I include src
