-module(wgconfig_storage).
-behavior(gen_server).

-export([start_link/0,
         list_sections/0, list_sections/1, list_keys/1,
         add_config/1,
         get/2, set/3,
         save_config_files/1, get_config_files/0,
         stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("otp_types.hrl").
-include("wgconfig.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {
          config_files = [] :: [file:name_all()]
         }).


%%% module API

-spec start_link() -> gs_start_link_reply().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec list_sections() -> [binary()].
list_sections() ->
    gen_server:call(?MODULE, list_sections).


-spec list_sections(wgconfig_name()) -> [binary()].
list_sections(Prefix) ->
    BinPrefix = to_bin(Prefix),
    Size = byte_size(BinPrefix),
    AllSections = gen_server:call(?MODULE, list_sections),
    lists:filter(fun(<<Start:Size/binary, _Rest/binary>>) -> Start =:= BinPrefix;
                    (_) -> false
                 end, AllSections).

-spec list_keys(wgconfig_name()) -> [binary()].
list_keys(Section) ->
    BinSection = to_bin(Section),
    gen_server:call(?MODULE, {list_keys, BinSection}).


-spec add_config(wgconfig()) -> ok.
add_config(Config) ->
    gen_server:call(?MODULE, {add_config, Config}),
    ok.


-spec get(wgconfig_name(), wgconfig_name()) -> {ok, binary()} | {error, not_found}.
get(SectionName, Key) ->
    case persistent_term:get({?MODULE, to_bin(SectionName), to_bin(Key)},
                             {error, not_found}) of
        {error, not_found} ->
            {error, not_found};

        Value ->
            {ok, Value}
    end.


-spec set(wgconfig_name(), wgconfig_name(), binary()) -> ok.
set(SectionName, Key, Value) ->
    gen_server:call(?MODULE, {set, to_bin(SectionName), to_bin(Key), Value}),
    ok.


-spec save_config_files([file:name_all()]) -> ok.
save_config_files(Files) ->
    gen_server:cast(?MODULE, {save_config_files, Files}),
    ok.



-spec get_config_files() -> [file:name_all()].
get_config_files() ->
    gen_server:call(?MODULE, get_config_files).


-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop),
    ok.


%%% gen_server API

-spec init(gs_args()) -> gs_init_reply().
init([]) ->
    {ok, #state{}}.


-spec handle_call(gs_request(), gs_from(), gs_reply()) -> gs_call_reply().
handle_call({add_config, Config}, _From, State) ->
    maps:map(fun({SectionName, Key}, Value) ->
                    persistent_term:put({?MODULE, SectionName, Key}, Value),
                    Value
             end, Config),
    {reply, ok, State};

handle_call({set, SectionName, Key, Value}, _From, State) ->
    persistent_term:put({?MODULE, SectionName, Key}, Value),
    {reply, ok, State};

handle_call(list_sections, _From, State) ->
    {CompositeKeys, _Values} = lists:unzip(persistent_term:get()),

    SectionNames = lists:filtermap(
        fun
            ({?MODULE, SectionName, _Key}) ->
                {true, SectionName};

            (_) ->
                false
        end,
        CompositeKeys
    ),

    Names = sets:to_list(sets:from_list(SectionNames)),
    {reply, Names, State};

handle_call({list_keys, Section}, _From, State) ->
    {CompositeKeys, _Values} = lists:unzip(persistent_term:get()),

    ExtractKeys = fun
        ({?MODULE, SectionName, Key}) ->
            case SectionName of
                Section ->
                    {true, Key};

                _ ->
                    false
            end;

        (_) ->
            false
    end,

    Keys = lists:filtermap(
        ExtractKeys,
        CompositeKeys
    ),

    {reply, Keys, State};

handle_call(get_config_files, _From, #state{config_files = Files} = State) ->
    {reply, Files, State};

handle_call(_Any, _From, State) ->
    {noreply, State}.


-spec handle_cast(gs_request(), gs_state()) -> gs_cast_reply().
handle_cast({save_config_files, Files}, State) ->
    {noreply, State#state{config_files = Files}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Any, State) ->
    {noreply, State}.


-spec handle_info(gs_request(), gs_state()) -> gs_info_reply().
handle_info(_Any, State) ->
    {noreply, State}.


-spec terminate(terminate_reason(), gs_state()) -> ok.
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(), term(), term()) -> gs_code_change_reply().
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.



%%% inner functions

-spec to_bin(wgconfig_name()) -> binary().
to_bin(Name) when is_atom(Name) ->
    unicode:characters_to_binary(atom_to_list(Name));
to_bin(Name) when is_list(Name) ->
    unicode:characters_to_binary(Name);
to_bin(Name) when is_binary(Name) ->
    Name.
