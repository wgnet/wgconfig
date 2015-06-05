-module(wgconfig_tests).

-include_lib("eunit/include/eunit.hrl").

base_api_test() ->
    application:start(wgconfig),
    wgconfig:load_configs(["../test/my_config.ini", "../test/my_config2.ini"]),

    ?assertEqual({ok, <<"test_db">>},
                 wgconfig:get(<<"database">>, <<"db_name">>)),

    ?assertEqual({ok, <<"my_user">>},
                 wgconfig:get(<<"database">>, <<"db_user">>)),

    ?assertEqual({ok, <<"200">>},
                 wgconfig:get(<<"workers_pool">>, <<"num_workers">>)),

    ?assertEqual({ok, <<"300">>},
                 wgconfig:get(<<"workers_pool">>, <<"max_workers">>)),

    ?assertEqual({error,not_found},
                 wgconfig:get(<<"some_section">>, <<"port">>)),

    application:stop(wgconfig),
    ok.


get_bool_test() ->
    application:start(wgconfig),
    wgconfig:load_configs(["../test/my_config3.ini"]),

    ?assertEqual(true, wgconfig:get_bool(<<"section 1">>, <<"key_1">>)),
    ?assertEqual(false, wgconfig:get_bool(<<"section 1">>, <<"key_2">>)),
    ?assertEqual(true, wgconfig:get_bool(<<"section 2">>, <<"key_1">>)),
    ?assertEqual(false, wgconfig:get_bool(<<"section 2">>, <<"key_2">>)),
    ?assertEqual(true, wgconfig:get_bool(<<"section 2">>, <<"key_3">>)),
    ?assertEqual(false, wgconfig:get_bool(<<"section 2">>, <<"key_4">>)),

    ?assertEqual(true, wgconfig:get_bool(<<"section 1">>, <<"key_0">>, true)),
    ?assertEqual(false, wgconfig:get_bool(<<"section 1">>, <<"key_0">>, false)),

    ?assertThrow({wgconfig_error, <<"invalid bool 'hello'">>},
                  wgconfig:get_bool(<<"section 1">>, <<"key_3">>)),
    ?assertThrow({wgconfig_error, value_not_found},
                  wgconfig:get_bool(<<"section 1">>, <<"key_0">>)),

    application:stop(wgconfig),
    ok.


get_int_test() ->
    application:start(wgconfig),
    wgconfig:load_configs(["../test/my_config3.ini"]),

    ?assertEqual(77, wgconfig:get_int(<<"section 1">>, <<"key_4">>)),
    ?assertEqual(0, wgconfig:get_int(<<"section 3">>, <<"key_5">>)),
    ?assertEqual(-100, wgconfig:get_int(<<"section 3">>, <<"key_4">>)),

    ?assertEqual(42, wgconfig:get_int(<<"section 1">>, <<"key_0">>, 42)),
    ?assertEqual(-999, wgconfig:get_int(<<"section 0">>, <<"key_1">>, -999)),

    ?assertThrow({wgconfig_error, <<"invalid int 'hello'">>},
                  wgconfig:get_int(<<"section 1">>, <<"key_3">>)),
    ?assertThrow({wgconfig_error, value_not_found},
                  wgconfig:get_int(<<"section 1">>, <<"key_0">>)),

    application:stop(wgconfig),
    ok.


get_float_test() ->
    application:start(wgconfig),
    wgconfig:load_configs(["../test/my_config3.ini"]),

    ?assertEqual(77.77, wgconfig:get_float(<<"section 1">>, <<"key_5">>)),
    ?assertEqual(3.14159265, wgconfig:get_float(<<"section 3">>, <<"key_6">>)),
    ?assertEqual(-3.14159265, wgconfig:get_float(<<"section 3">>, <<"key_7">>)),

    ?assertEqual(42.0, wgconfig:get_float(<<"section 1">>, <<"key_0">>, 42.0)),
    ?assertEqual(0.123, wgconfig:get_float(<<"section 0">>, <<"key_1">>, 0.123)),

    ?assertThrow({wgconfig_error, <<"invalid float 'a,b,c,d'">>},
                  wgconfig:get_float(<<"section 3">>, <<"key_2">>)),
    ?assertThrow({wgconfig_error, value_not_found},
                  wgconfig:get_float(<<"section 1">>, <<"key_0">>)),

    application:stop(wgconfig),
    ok.


%% get_bool_test() ->
%%     application:start(wgconfig),
%%     wgconfig:load_configs(["../test/my_config3.ini"]),

%%     application:stop(wgconfig),
%%     ok.
