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


parse_key_value_test() ->
    ?assertEqual({<<"key">>, <<"value">>}, wgconfig_parser:parse_key_value(<<"key=value">>)),
    ?assertEqual({<<"key">>, <<"value">>}, wgconfig_parser:parse_key_value(<<"key = value">>)),
    ?assertEqual({<<"key">>, <<"value">>}, wgconfig_parser:parse_key_value(<<"key = value # comment">>)),
    ?assertEqual({<<"some_key">>, <<"15">>}, wgconfig_parser:parse_key_value(<<" some_key = 15   # comment">>)),
    ?assertEqual({<<"other_key">>, <<"77">>}, wgconfig_parser:parse_key_value(<<"  other_key = 77   #">>)),
    ?assertEqual(skip, wgconfig_parser:parse_key_value(<<"#  other_key = 77">>)),
    ?assertEqual(skip, wgconfig_parser:parse_key_value(<<"  #  other_key = 77">>)),
    ?assertEqual(skip, wgconfig_parser:parse_key_value(<<" hello there">>)),
    ok.


parse_line_test() ->
    S0 = [],
    S1 = [{<<"some_section">>, []}],
    ?assertEqual(S1, wgconfig_parser:parse_line(<<"[some_section]">>, S0)),

    S2 = [{<<"some_section">>, [{<<"key">>, <<"value">>}]}],
    ?assertEqual(S2, wgconfig_parser:parse_line(<<" key = value # comment">>, S1)),

    ?assertEqual([], wgconfig_parser:parse_line(<<" key = value # comment">>, S0)),

    S3 = [{<<"some_section">>, [{<<"port">>, <<"5432">>}, {<<"key">>, <<"value">>}]}],
    ?assertEqual(S3, wgconfig_parser:parse_line(<<"port = 5432">>, S2)),

    S4 = [{<<"other_section">>, []},
          {<<"some_section">>, [{<<"port">>, <<"5432">>}, {<<"key">>, <<"value">>}]}],
    ?assertEqual(S4, wgconfig_parser:parse_line(<<" [  other_section ]  ">>, S3)),

    S5 = [{<<"other_section">>, [{<<"host">>, <<"localhost">>}]},
          {<<"some_section">>, [{<<"port">>, <<"5432">>}, {<<"key">>, <<"value">>}]}],
    ?assertEqual(S5, wgconfig_parser:parse_line(<<" host=localhost">>, S4)),

    S6 = [{<<"other_section">>, [{<<"enabled">>, <<"true">>}, {<<"host">>, <<"localhost">>}]},
          {<<"some_section">>, [{<<"port">>, <<"5432">>}, {<<"key">>, <<"value">>}]}],
    ?assertEqual(S6, wgconfig_parser:parse_line(<<" enabled =   true">>, S5)),

    ?assertEqual(S0, wgconfig_parser:parse_line(<<"any line">>, S0)),
    ?assertEqual(S1, wgconfig_parser:parse_line(<<"# comment line">>, S1)),
    ?assertEqual(S2, wgconfig_parser:parse_line(<<" # key = value">>, S2)),
    ?assertEqual(S3, wgconfig_parser:parse_line(<<"not a section]">>, S3)),
    ?assertEqual(S4, wgconfig_parser:parse_line(<<"">>, S4)),
    ?assertEqual(S5, wgconfig_parser:parse_line(<<" ">>, S5)),
    ?assertEqual(S6, wgconfig_parser:parse_line(<<" # [commented_section] ">>, S6)),

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
    Res = [{<<"Section 2">>, [{<<"num_workers">>, <<"50">>}]},
           {<<"Section 1">>, [{<<"port">>, <<"5432">>}, {<<"host">>, <<"localhost">>}]}],
    ?assertEqual(Res, wgconfig_parser:parse_bin(Bin)),
    ok.


parse_file_test() ->
    Res = [{<<"http_client">>, [{<<"max_retries">>, <<"7">>},
                                {<<"retry_time">>, <<"1000">>},
                                {<<"http_timeout">>, <<"5000">>}]},
           {<<"workers_pool">>, [{<<"max_workers">>, <<"30">>},
                                 {<<"num_workers">>, <<"20">>}]},
           {<<"database">>, [{<<"db_user">>, <<"my_user">>},
                             {<<"db_name">>, <<"my_db">>},
                             {<<"port">>, <<"5432">>},
                             {<<"host">>, <<"localhost">>}]}],
    ?assertEqual({ok, Res}, wgconfig_parser:parse_file("../test/my_config.ini")),
    ?assertEqual({error, enoent}, wgconfig_parser:parse_file("../test/other_config.ini")),
    ok.
