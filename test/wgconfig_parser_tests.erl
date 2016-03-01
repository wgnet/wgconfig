-module(wgconfig_parser_tests).

-include_lib("eunit/include/eunit.hrl").

trim_test() ->
    ?assertEqual(<<"ok">>, wgconfig_parser:trim(<<"ok">>)),
    ?assertEqual(<<"ok">>, wgconfig_parser:trim(<<" ok">>)),
    ?assertEqual(<<"ok">>, wgconfig_parser:trim(<<"   ok">>)),
    ?assertEqual(<<"ok">>, wgconfig_parser:trim(<<"ok ">>)),
    ?assertEqual(<<"ok">>, wgconfig_parser:trim(<<"ok  ">>)),
    ?assertEqual(<<"ok">>, wgconfig_parser:trim(<<" ok ">>)),
    ?assertEqual(<<"ok">>, wgconfig_parser:trim(<<"   ok    ">>)),
    ?assertEqual(<<"#comment">>, wgconfig_parser:trim(<<" #comment  ">>)),
    ?assertEqual(<<"some words with spaces">>, wgconfig_parser:trim(<<"some words with spaces">>)),
    ?assertEqual(<<"some words with spaces">>, wgconfig_parser:trim(<<"    some words with spaces">>)),
    ?assertEqual(<<"some words with spaces">>, wgconfig_parser:trim(<<"some words with spaces    ">>)),
    ?assertEqual(<<"some words with spaces">>, wgconfig_parser:trim(<<" some words with spaces  ">>)),
    ok.


parse_line_test() ->
    S0 = {<<"default">>, #{}},
    S1 = {<<"some_section">>, #{}},
    ?assertEqual(S1, wgconfig_parser:parse_line(<<"[some_section]">>, S0)),

    S2 = {<<"some_section">>, #{{<<"some_section">>, <<"key">>} => <<"value">>}},
    ?assertEqual(S2, wgconfig_parser:parse_line(<<" key = value # comment">>, S1)),

    S3 = {<<"some_section">>,
          #{{<<"some_section">>, <<"key">>} => <<"value">>,
            {<<"some_section">>, <<"port">>} => <<"5432">>
           }},
    ?assertEqual(S3, wgconfig_parser:parse_line(<<"port = 5432">>, S2)),

    S4 = {<<"other_section">>,
          #{{<<"some_section">>, <<"key">>} => <<"value">>,
            {<<"some_section">>, <<"port">>} => <<"5432">>
           }},
    ?assertEqual(S4, wgconfig_parser:parse_line(<<" [  other_section ]  ">>, S3)),

    S5 = {<<"other_section">>,
          #{{<<"some_section">>, <<"key">>} => <<"value">>,
            {<<"some_section">>, <<"port">>} => <<"5432">>,
            {<<"other_section">>, <<"host">>} => <<"localhost">>
           }},
    ?assertEqual(S5, wgconfig_parser:parse_line(<<" host=localhost">>, S4)),

    S6 = {<<"other_section">>,
          #{{<<"some_section">>, <<"key">>} => <<"value">>,
            {<<"some_section">>, <<"port">>} => <<"5432">>,
            {<<"other_section">>, <<"host">>} => <<"localhost">>,
            {<<"other_section">>, <<"enabled">>} => <<"true">>
           }},
    ?assertEqual(S6, wgconfig_parser:parse_line(<<" enabled =   true">>, S5)),

    ?assertEqual(S0, wgconfig_parser:parse_line(<<"any line">>, S0)),
    ?assertEqual(S1, wgconfig_parser:parse_line(<<"# comment line">>, S1)),
    ?assertEqual(S2, wgconfig_parser:parse_line(<<" # key = value">>, S2)),
    ?assertEqual(S3, wgconfig_parser:parse_line(<<"not a section]">>, S3)),
    ?assertEqual(S4, wgconfig_parser:parse_line(<<"">>, S4)),
    ?assertEqual(S5, wgconfig_parser:parse_line(<<" ">>, S5)),
    ?assertEqual(S6, wgconfig_parser:parse_line(<<" # [commented_section] ">>, S6)),

    ok.


parse_quoted_value_test() ->
    S1 = {<<"some_section">>, #{}},
    S2 = {<<"some_section">>, #{{<<"some_section">>, <<"key">>} => <<"ta#ta#ta">>}},
    ?assertEqual(S2, wgconfig_parser:parse_line(<<" key = \"ta#ta#ta\"">>, S1)),
    S3 = {<<"some_section">>, #{{<<"some_section">>, <<"key">>} => <<"tatata">>}},
    ?assertEqual(S3, wgconfig_parser:parse_line(<<" key = \"tatata\" # comment">>, S1)),
    S4 = {<<"some_section">>, #{{<<"some_section">>, <<"key">>} => <<"#not#a#comment#">>}},
    ?assertEqual(S4, wgconfig_parser:parse_line(<<" key = \"#not#a#comment#\" # comment">>, S1)),

    ?assertEqual(S2, wgconfig_parser:parse_line(<<" key = 'ta#ta#ta'">>, S1)),
    ?assertEqual(S3, wgconfig_parser:parse_line(<<" key = 'tatata' # comment">>, S1)),
    ?assertEqual(S4, wgconfig_parser:parse_line(<<" key = '#not#a#comment#' # comment">>, S1)),

    S5 = {<<"some_section">>, #{{<<"some_section">>, <<"key">>} => <<"ta\"ta">>}},
    ?assertEqual(S5, wgconfig_parser:parse_line(<<" key = 'ta\"ta' # comment">>, S1)),
    S6 = {<<"some_section">>, #{{<<"some_section">>, <<"key">>} => <<"ta'ta">>}},
    ?assertEqual(S6, wgconfig_parser:parse_line(<<" key = \"ta'ta\" # comment">>, S1)),
    ok.


parse_bin_test() ->
    Bin = <<" # My cool config \n",
            "[Section 1] \n",
            "host = localhost \n",
            "port = 5432 \n",
            "\n"
            "[Section 2]\n",
            " # enabled = true \n",
            "num_workers =  50 # workers pool \n",
            "\n">>,
    Res = #{{<<"Section 2">>, <<"num_workers">>} => <<"50">>,
            {<<"Section 1">>, <<"port">>} => <<"5432">>,
            {<<"Section 1">>, <<"host">>} => <<"localhost">>},
    ?assertEqual(Res, wgconfig_parser:parse_bin(Bin)),
    ok.


parse_file_test() ->
    Res = #{{<<"lager.crash">>, <<"enabled">>} => <<"true">>,
            {<<"lager.handlers.warning">>, <<"enabled">>} => <<"true">>,
            {<<"lager.handlers.info">>, <<"enabled">>} => <<"true">>,
            {<<"lager.handlers.error">>, <<"enabled">>} => <<"true">>,
            {<<"http_client">>, <<"max_retries">>} => <<"7">>,
            {<<"http_client">>, <<"retry_time">>} => <<"1000">>,
            {<<"http_client">>, <<"http_timeout">>} => <<"5000">>,
            {<<"workers_pool">>, <<"max_workers">>} => <<"30">>,
            {<<"workers_pool">>, <<"num_workers">>} => <<"20">>,
            {<<"database">>, <<"db_user">>} => <<"my_user">>,
            {<<"database">>, <<"db_name">>} => <<"my_db">>,
            {<<"database">>, <<"port">>} => <<"5432">>,
            {<<"database">>, <<"host">>} => <<"localhost">>
           },
    application:ensure_all_started(wgconfig),
    file:set_cwd(code:lib_dir(wgconfig)),
    ?assertEqual({ok, Res}, wgconfig_parser:parse_file("./test/data/my_config.ini")),
    ?assertEqual({error, enoent}, wgconfig_parser:parse_file("./test/data/other_config.ini")),
    ok.
