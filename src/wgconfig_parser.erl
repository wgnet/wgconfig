-module(wgconfig_parser).

-include("wgconfig.hrl").

-export([parse_file/1, parse_bin/1]).

%% export for unit tests
-export([parse_line/2, parse_key_value/1, trim/1]).


-spec parse_file(file:name_all()) -> {ok, [section()]} | {error, atom()}.
parse_file(FileName) ->
    case file:read_file(FileName) of
        {ok, Bin} -> {ok, parse_bin(Bin)};
        {error, Reason} -> {error, Reason}
    end.


-spec parse_bin(binary()) -> [section()].
parse_bin(Bin) ->
    Lines = binary:split(Bin, [<<"\n">>, <<"\r">>], [global]),
    lists:foldl(fun parse_line/2, [], Lines).


-spec parse_line(binary(), [section()]) -> [section()].
parse_line(Line, Sections) ->
    case binary:split(Line, [<<"[">>]) of
        [Before, Rest] -> % line: [section name]
            case trim(Before) of
                <<"#", _/binary>> -> Sections; % skip commented section
                _ -> NewName = hd(binary:split(Rest, [<<"]">>])),
                     NewSection = {trim(NewName), []},
                     [NewSection | Sections]
            end;
        [KeyValue] when length(Sections) > 0 -> % line: key = value # comment
            case parse_key_value(KeyValue) of
                {Key, Value} ->
                    [CurrSection | RestSections] = Sections,
                    {Name, KVs} = CurrSection,
                    [{Name, [{Key, Value} | KVs]} | RestSections];
                skip -> Sections
            end;
        _ -> Sections % line: any other
    end.


-spec parse_key_value(binary()) -> key_value().
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


-spec trim(binary()) -> binary().
trim(Bin) ->
    Str = unicode:characters_to_list(Bin),
    Str2 = string:strip(Str),
    unicode:characters_to_binary(Str2).
