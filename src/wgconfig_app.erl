-module(wgconfig_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).
-include("logger.hrl").


-spec(start() -> ok).
start() ->
    lager:start(),
    application:start(wgconfig),
    ok.


-spec(start(term(), term()) -> {ok, pid()}).
start(_StartType, _StartArgs) ->
    ?INFO("start some_app"),
    wgconfig_sup:start_link().


-spec(stop(term()) -> ok).
stop(_State) ->
    ok.
