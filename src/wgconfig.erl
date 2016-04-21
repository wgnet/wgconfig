-module(wgconfig).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-include("wgconfig.hrl").

-export([load_configs/1, load_config/1,
         reload/0, subscribe/1, subscribe/2,
         get/2, get/3, get/4, set/3,
         list_sections/0, list_sections/1, list_keys/1,
         get_bool/2, get_bool/3,
         get_int/2, get_int/3,
         get_float/2, get_float/3,
         get_string/2, get_string/3,
         get_binary/2, get_binary/3,
         get_string_list/2, get_string_list/3,
         get_binary_list/2, get_binary_list/3
        ]).


%%% module API

-spec load_configs([file:name_all()]) -> [ok | {error, atom()}].
load_configs(ConfigFiles) ->
    wgconfig_storage:save_config_files(ConfigFiles),
    lists:map(fun load_config/1, ConfigFiles).


-spec reload() -> [ok | {error, atom()}].
reload() ->
    ConfigFiles = wgconfig_storage:get_config_files(),
    Res = lists:map(fun load_config/1, ConfigFiles),
    gen_event:notify(wgconfig_event_manager, wgconfig_reload),
    Res.


-spec load_config(file:name_all()) -> ok | {error, atom()}.
load_config(FileName) ->
    case wgconfig_parser:parse_file(FileName) of
        {ok, Config} -> wgconfig_storage:add_config(Config),
                          ok;
        {error, Reason} -> {error, Reason}
    end.


-spec subscribe(module()) -> ok.
subscribe(Module) ->
    subscribe(Module, []).


-spec subscribe(module(), list()) -> ok.
subscribe(Module, Args) ->
    gen_event:add_handler(wgconfig_event_manager, Module, Args),
    ok.


-spec get(wgconfig_name(), wgconfig_name()) -> {ok, binary()} | {error, not_found}.
get(SectionName, Key) ->
    wgconfig_storage:get(SectionName, Key).


-spec get(wgconfig_name(), wgconfig_name(), function()) -> term().
get(SectionName, Key, Cast) ->
    case wgconfig_storage:get(SectionName, Key) of
        {ok, Value} ->
            try
                Cast(Value)
            catch
                throw:{wgconfig_error, Error} ->
                    throw({wgconfig_error, Error, SectionName, Key})
            end;
        {error, not_found} ->
            throw({wgconfig_error, value_not_found, SectionName, Key})
    end.


-spec get(wgconfig_name(), wgconfig_name(), term(), function()) -> term().
get(SectionName, Key, Default, Cast) ->
    case wgconfig_storage:get(SectionName, Key) of
        {ok, Value} ->
            try
                Cast(Value)
            catch
                throw:{wgconfig_error, Error} ->
                    throw({wgconfig_error, Error, SectionName, Key})
            end;
        {error, not_found} -> Default
    end.


-spec set(wgconfig_name(), wgconfig_name(), term()) -> ok.
set(SectionName, Key, true) ->
    set(SectionName, Key, <<"true">>);
set(SectionName, Key, false) ->
    set(SectionName, Key, <<"false">>);
set(SectionName, Key, Value) when is_integer(Value) ->
    set(SectionName, Key, integer_to_binary(Value));
set(SectionName, Key, Value) when is_float(Value) ->
    set(SectionName, Key, float_to_binary(Value));
set(SectionName, Key, Value) when is_list(Value) ->
    set(SectionName, Key, unicode:characters_to_binary(Value));
set(SectionName, Key, Value) when is_binary(Value) ->
    wgconfig_storage:set(SectionName, Key, Value).


-spec list_sections() -> [wgconfig_section()].
list_sections() ->
    wgconfig_storage:list_sections().


-spec list_sections(wgconfig_name()) -> [wgconfig_section()].
list_sections(Prefix) ->
    wgconfig_storage:list_sections(Prefix).

-spec list_keys(wgconfig_name()) -> [binary()].
list_keys(Section) ->
    wgconfig_storage:list_keys(Section).

-spec get_bool(wgconfig_name(), wgconfig_name()) -> true | false.
get_bool(SectionName, Key) ->
    get(SectionName, Key, fun value_to_bool/1).


-spec get_bool(wgconfig_name(), wgconfig_name(), true | false) -> true | false.
get_bool(SectionName, Key, Default) ->
    get(SectionName, Key, Default, fun value_to_bool/1).


-spec get_int(wgconfig_name(), wgconfig_name()) -> integer().
get_int(SectionName, Key) ->
    get(SectionName, Key, fun value_to_int/1).


get_int(SectionName, Key, Default) ->
    get(SectionName, Key, Default, fun value_to_int/1).


-spec get_float(wgconfig_name(), wgconfig_name()) -> float().
get_float(SectionName, Key) ->
    get(SectionName, Key, fun value_to_float/1).


-spec get_float(wgconfig_name(), wgconfig_name(), float()) -> float().
get_float(SectionName, Key, Default) ->
    get(SectionName, Key, Default, fun value_to_float/1).


-spec get_string(wgconfig_name(), wgconfig_name()) -> string().
get_string(SectionName, Key) ->
    get(SectionName, Key, fun unicode:characters_to_list/1).


-spec get_string(wgconfig_name(), wgconfig_name(), string()) -> string().
get_string(SectionName, Key, Default) ->
    get(SectionName, Key, Default, fun unicode:characters_to_list/1).


-spec get_binary(wgconfig_name(), wgconfig_name()) -> binary().
get_binary(SectionName, Key) ->
    get(SectionName, Key, fun(B) -> B end).


-spec get_binary(wgconfig_name(), wgconfig_name(), binary()) -> binary().
get_binary(SectionName, Key, Default) ->
    get(SectionName, Key, Default, fun(B) -> B end).


-spec get_string_list(wgconfig_name(), wgconfig_name()) -> [binary()].
get_string_list(SectionName, Key) ->
    get(SectionName, Key, fun value_to_string_list/1).


-spec get_string_list(wgconfig_name(), wgconfig_name(), [binary()]) -> [binary()].
get_string_list(SectionName, Key, Default) ->
    get(SectionName, Key, Default, fun value_to_string_list/1).


-spec get_binary_list(wgconfig_name(), wgconfig_name()) -> [binary()].
get_binary_list(SectionName, Key) ->
    get(SectionName, Key, fun value_to_binary_list/1).


-spec get_binary_list(wgconfig_name(), wgconfig_name(), [binary()]) -> [binary()].
get_binary_list(SectionName, Key, Default) ->
    get(SectionName, Key, Default, fun value_to_binary_list/1).


%% inner functions

-spec value_to_bool(binary()) -> true | false.
value_to_bool(<<"true">>) -> true;
value_to_bool(<<"false">>) -> false;
value_to_bool(<<"on">>) -> true;
value_to_bool(<<"off">>) -> false;
value_to_bool(<<"yes">>) -> true;
value_to_bool(<<"no">>) -> false;
value_to_bool(Value) -> throw({wgconfig_error, <<"invalid bool '", Value/binary, "'">>}).


-spec value_to_int(binary()) -> integer().
value_to_int(Value) ->
    case string:to_integer(binary_to_list(Value)) of
        {Int, []} -> Int;
        _ -> throw({wgconfig_error, <<"invalid int '", Value/binary, "'">>})
    end.


-spec value_to_float(binary()) -> float().
value_to_float(Value) ->
    case string:to_float(binary_to_list(Value)) of
        {Float, []} -> Float;
        _ -> throw({wgconfig_error, <<"invalid float '", Value/binary, "'">>})
    end.


-spec value_to_string_list(binary()) -> [binary()].
value_to_string_list(Value) ->
    Values = binary:split(Value, [<<",">>], [global]),
    lists:filtermap(fun(Bin) ->
                            Str = unicode:characters_to_list(Bin),
                            case string:strip(Str) of
                                "" -> false;
                                Str2 -> {true, Str2}
                            end
                    end, Values).


-spec value_to_binary_list(binary()) -> [binary()].
value_to_binary_list(Value) ->
    Values = binary:split(Value, [<<",">>], [global]),
    Values2 = lists:map(fun wgconfig_parser:trim/1, Values),
    lists:filter(fun(<<>>) -> false;
                    (_) -> true
                 end, Values2).
