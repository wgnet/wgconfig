-module(sample_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, terminate/2]).


init(_Args) ->
    {ok, []}.


handle_event(Event, State) ->
    ct:pal("sample_handler got event: ~p", [Event]),
    {ok, [Event | State]}.


handle_call(get_state, State) ->
    {ok, State, State}.


terminate(_Args, _State) ->
    ok.
