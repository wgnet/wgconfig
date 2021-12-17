REBARVER = 3.15.2
ifeq ($(OTPVER),24.0)
	REBARVER = 3.17.0
endif


compile:
	rebar3 compile


eunit:
	rebar3 eunit


ct:
	rebar3 ct


console:
	erl -pa _build/default/lib/*/ebin -config sys -s wgconfig_app start


d:
	rebar3 dialyzer


clean-all:
	-rm -rf _build
	-rm -f erl_crash.dump
	-rm -f rebar.lock


rebar3:
	wget https://github.com/erlang/rebar3/releases/download/${REBARVER}/rebar3 &&\
	chmod u+x rebar3
