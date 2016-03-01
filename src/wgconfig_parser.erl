-module(wgconfig_parser).

-include("wgconfig.hrl").

-export([parse_file/1, parse_bin/1, trim/1]).
-export([parse_line/2, parse_key_value/1]). % export for unit tests


%% Module API

-spec parse_file(file:name_all()) -> {ok, wgconfig()} | {error, atom()}.
parse_file(FileName) ->
    case file:read_file(FileName) of
        {ok, Bin} -> {ok, parse_bin(Bin)};
        {error, Reason} -> {error, Reason}
    end.


-spec parse_bin(binary()) -> wgconfig().
parse_bin(Bin) ->
    Lines = binary:split(Bin, [<<"\n">>, <<"\r">>], [global]),
    {_, Config} = lists:foldl(fun parse_line/2, {<<"default">>, maps:new()}, Lines),
    Config.


-spec trim(binary()) -> binary().
trim(Bin) ->
    Str = unicode:characters_to_list(Bin),
    Str2 = string:strip(Str),
    unicode:characters_to_binary(Str2).


%% inner functions

-spec parse_line(binary(), {wgconfig_section(), wgconfig()}) -> {wgconfig_section(), wgconfig()}.
parse_line(Line, {CurrentSection, Config}) ->
    case binary:split(Line, [<<"[">>]) of
        [Before, Rest] -> % line: [section name]
            case trim(Before) of
                <<"#", _/binary>> -> {CurrentSection, Config}; % skip commented section
                _ -> NewSection = trim(hd(binary:split(Rest, [<<"]">>]))),
                     {NewSection, Config}
            end;
        [KeyValue] -> % line: key = value # comment
            case parse_key_value(KeyValue) of
                {Key, Value} ->
                    FullKey = {CurrentSection, Key},
                    {CurrentSection, Config#{FullKey => Value}};
                skip ->
                    {CurrentSection, Config}
            end;
        _ -> % line: any other
            {CurrentSection, Config}
    end.


-spec parse_key_value(binary()) -> binary() | skip.
parse_key_value(Bin) ->
    case binary:split(Bin, [<<"=">>]) of
        [Key, Rest] ->
            Key2 = trim(Key),
            case Key2 of
                <<"#", _/binary>> -> skip; % skip commented key
                _ -> Value = hd(binary:split(Rest, [<<"#">>])),
                     {Key2, trim(Value)}
            end;
        _ -> skip
    end.
