-module(wgconfig_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-include("otp_types.hrl").


-spec(start_link() -> {ok, pid()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec(init(gs_args()) -> sup_init_reply()).
init([]) ->
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent, % permanent | transient | temporary
    Shutdown = 2000,     % brutal_kill | int() >= 0 | infinity

    SomeWorker = {some_worker,
		  {some_worker, start_link, []},
		  Restart, Shutdown, worker,
		  [some_worker]},

    {ok, {SupFlags, [SomeWorker]}}.
