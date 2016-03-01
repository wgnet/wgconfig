-module(wgconfig_parser).

-include("wgconfig.hrl").

-export([parse_file/1, parse_bin/1]).
-export([trim/1, parse_line/2]). % export for unit tests


%%% Module API

-spec parse_file(file:name_all()) -> {ok, wgconfig()} | {error, atom()}.
parse_file(FileName) ->
    case file:read_file(FileName) of
        {ok, Bin} -> {ok, parse_bin(Bin)};
        {error, Reason} -> {error, Reason}
    end.


-spec parse_bin(binary()) -> wgconfig().
parse_bin(Bin) ->
    Lines = binary:split(Bin, [<<"\n">>, <<"\r">>], [global]),
    REs = re_list(), % compile REs once outside foldl
    {_, Config} = lists:foldl(fun(Line, Acc) -> match(REs, Line, Acc) end,
                              {<<"default">>, maps:new()},
                              Lines),
    Config.


-spec trim(binary()) -> binary().
trim(Bin) ->
    Str = unicode:characters_to_list(Bin),
    Str2 = string:strip(Str),
    unicode:characters_to_binary(Str2).

%% exported for unit tests
-spec parse_line(binary(), {wgconfig_section(), wgconfig()}) -> {wgconfig_section(), wgconfig()}.
parse_line(Line, {CurrentSection, Config}) ->
    match(re_list(), Line, {CurrentSection, Config}).


%%% inner functions

-spec re_list() -> [re:mp()].
re_list() ->
    lists:map(fun({ok, RE}) -> RE end,
              [
               re:compile(<<"^\s*\\[([^\\]]+)\\]">>)       % [section name]
              ,re:compile(<<"^([^=#]+)=\s*\"([^\"]+)\"">>) % key = "value" # comment
              ,re:compile(<<"^([^=#]+)=\s*'([^']+)'">>)    % key = 'value' # comment
              ,re:compile(<<"^([^=#]+)=([^#]+)">>)         % key =  value  # comment
              ]).


match([], _Line, Res) -> Res;
match([RE | Rest], Line, {CurrentSection, Config}) ->
    case re:run(Line, RE) of
        {match, [_, PosLen]} ->
            NewSection = trim(binary:part(Line, PosLen)),
            {NewSection, Config};
        {match, [_, PosLen1, PosLen2]} ->
            Key = trim(binary:part(Line, PosLen1)),
            Value = trim(binary:part(Line, PosLen2)),
            FullKey = {CurrentSection, Key},
            {CurrentSection, Config#{FullKey => Value}};
        nomatch -> match(Rest, Line, {CurrentSection, Config})
    end.
