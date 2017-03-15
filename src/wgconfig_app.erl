-module(wgconfig_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).

-spec start() -> ok.
start() ->
    application:start(wgconfig),
    ok.


-spec start(term(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    Res = wgconfig_sup:start_link(),
    case application:get_env(wgconfig, load_configs) of
        {ok, FileNames} when is_list(FileNames) -> wgconfig:load_configs(FileNames);
        _ -> do_nothing
    end,
    Res.


-spec stop(term()) -> ok.
stop(_State) ->
    ok.
