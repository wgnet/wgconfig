-module(wgconfig_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    application:ensure_all_started(wgconfig),
    file:set_cwd(code:lib_dir(wgconfig)),
    ok.

base_api_test() ->
    setup(),
    Res = wgconfig:load_configs(["./test/data/my_config.ini", "./test/data/my_config2.ini"]),
    ?assertEqual([ok, ok], Res),

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
    setup(),
    wgconfig:load_configs(["./test/data/my_config3.ini"]),

    ?assertEqual(true, wgconfig:get_bool(<<"section 1">>, <<"key_1">>)),
    ?assertEqual(false, wgconfig:get_bool(<<"section 1">>, <<"key_2">>)),
    ?assertEqual(true, wgconfig:get_bool(<<"section 2">>, <<"key_1">>)),
    ?assertEqual(false, wgconfig:get_bool(<<"section 2">>, <<"key_2">>)),
    ?assertEqual(true, wgconfig:get_bool(<<"section 2">>, <<"key_3">>)),
    ?assertEqual(false, wgconfig:get_bool(<<"section 2">>, <<"key_4">>)),

    ?assertEqual(true, wgconfig:get_bool(<<"section 1">>, <<"key_0">>, true)),
    ?assertEqual(false, wgconfig:get_bool(<<"section 1">>, <<"key_0">>, false)),

    ?assertThrow({wgconfig_error, <<"invalid bool 'hello'">>, <<"section 1">>, <<"key_3">>},
                  wgconfig:get_bool(<<"section 1">>, <<"key_3">>)),
    ?assertThrow({wgconfig_error, value_not_found, <<"section 1">>, <<"key_0">>},
                  wgconfig:get_bool(<<"section 1">>, <<"key_0">>)),

    application:stop(wgconfig),
    ok.


get_int_test() ->
    setup(),
    wgconfig:load_configs(["./test/data/my_config3.ini"]),

    ?assertEqual(77, wgconfig:get_int(<<"section 1">>, <<"key_4">>)),
    ?assertEqual(0, wgconfig:get_int(<<"section 3">>, <<"key_5">>)),
    ?assertEqual(-100, wgconfig:get_int(<<"section 3">>, <<"key_4">>)),

    ?assertEqual(42, wgconfig:get_int(<<"section 1">>, <<"key_0">>, 42)),
    ?assertEqual(-999, wgconfig:get_int(<<"section 0">>, <<"key_1">>, -999)),

    ?assertThrow({wgconfig_error, <<"invalid int 'hello'">>, <<"section 1">>, <<"key_3">>},
                  wgconfig:get_int(<<"section 1">>, <<"key_3">>)),
    ?assertThrow({wgconfig_error, value_not_found, <<"section 1">>, <<"key_0">>},
                  wgconfig:get_int(<<"section 1">>, <<"key_0">>)),

    application:stop(wgconfig),
    ok.


get_float_test() ->
    setup(),
    wgconfig:load_configs(["./test/data/my_config3.ini"]),

    ?assertEqual(77.77, wgconfig:get_float(<<"section 1">>, <<"key_5">>)),
    ?assertEqual(3.14159265, wgconfig:get_float(<<"section 3">>, <<"key_6">>)),
    ?assertEqual(-3.14159265, wgconfig:get_float(<<"section 3">>, <<"key_7">>)),

    ?assertEqual(42.0, wgconfig:get_float(<<"section 1">>, <<"key_0">>, 42.0)),
    ?assertEqual(0.123, wgconfig:get_float(<<"section 0">>, <<"key_1">>, 0.123)),

    ?assertThrow({wgconfig_error, <<"invalid float 'a,b,c,d'">>, <<"section 3">>, <<"key_2">>},
                  wgconfig:get_float(<<"section 3">>, <<"key_2">>)),
    ?assertThrow({wgconfig_error, value_not_found, <<"section 1">>, <<"key_0">>},
                  wgconfig:get_float(<<"section 1">>, <<"key_0">>)),

    application:stop(wgconfig),
    ok.


get_string_test() ->
    setup(),
    wgconfig:load_configs(["./test/data/my_config3.ini"]),

    ?assertEqual("true", wgconfig:get_string(<<"section 1">>, <<"key_1">>)),
    ?assertEqual("false", wgconfig:get_string(<<"section 1">>, <<"key_2">>)),
    ?assertEqual("hello", wgconfig:get_string(<<"section 1">>, <<"key_3">>)),
    ?assertEqual("77", wgconfig:get_string(<<"section 1">>, <<"key_4">>)),
    ?assertEqual("77.77", wgconfig:get_string(<<"section 1">>, <<"key_5">>)),

    ?assertEqual("my string", wgconfig:get_string(<<"section 1">>, <<"key_0">>, "my string")),

    ?assertThrow({wgconfig_error, value_not_found, <<"section 1">>, <<"key_0">>},
                  wgconfig:get_string(<<"section 1">>, <<"key_0">>)),

    application:stop(wgconfig),
    ok.


get_binary_test() ->
    setup(),
    wgconfig:load_configs(["./test/data/my_config3.ini"]),

    ?assertEqual(<<"true">>, wgconfig:get_binary(<<"section 1">>, <<"key_1">>)),
    ?assertEqual(<<"false">>, wgconfig:get_binary(<<"section 1">>, <<"key_2">>)),
    ?assertEqual(<<"hello">>, wgconfig:get_binary(<<"section 1">>, <<"key_3">>)),
    ?assertEqual(<<"77">>, wgconfig:get_binary(<<"section 1">>, <<"key_4">>)),
    ?assertEqual(<<"77.77">>, wgconfig:get_binary(<<"section 1">>, <<"key_5">>)),

    ?assertEqual(<<"my string">>, wgconfig:get_binary(<<"section 1">>, <<"key_0">>, <<"my string">>)),

    ?assertThrow({wgconfig_error, value_not_found, <<"section 1">>, <<"key_0">>},
                  wgconfig:get_binary(<<"section 1">>, <<"key_0">>)),

    application:stop(wgconfig),
    ok.


get_string_list_test() ->
    setup(),
    wgconfig:load_configs(["./test/data/my_config3.ini"]),

    ?assertEqual(["hello"], wgconfig:get_string_list(<<"section 1">>, <<"key_3">>)),
    ?assertEqual(["one", "two", "three", "four"], wgconfig:get_string_list(<<"section 3">>, <<"key_1">>)),
    ?assertEqual(["a", "b", "c", "d"], wgconfig:get_string_list(<<"section 3">>, <<"key_2">>)),
    ?assertEqual(["a", "b", "c", "d"], wgconfig:get_string_list(<<"section 3">>, <<"key_3">>)),
    ?assertEqual(["-100"], wgconfig:get_string_list(<<"section 3">>, <<"key_4">>)),
    ?assertEqual(["bla bla", "bla bla bla", "tata ta ta"], wgconfig:get_string_list(<<"section 3">>, <<"key_8">>)),

    ?assertEqual(["my", "string"], wgconfig:get_string_list(<<"section 1">>, <<"key_0">>, ["my", "string"])),

    ?assertThrow({wgconfig_error, value_not_found, <<"section 1">>, <<"key_0">>},
                  wgconfig:get_string_list(<<"section 1">>, <<"key_0">>)),

    application:stop(wgconfig),
    ok.


get_binary_list_test() ->
    setup(),
    wgconfig:load_configs(["./test/data/my_config3.ini"]),

    ?assertEqual([<<"hello">>], wgconfig:get_binary_list(<<"section 1">>, <<"key_3">>)),
    ?assertEqual([<<"one">>, <<"two">>, <<"three">>, <<"four">>],
                 wgconfig:get_binary_list(<<"section 3">>, <<"key_1">>)),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>],
                 wgconfig:get_binary_list(<<"section 3">>, <<"key_2">>)),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>],
                 wgconfig:get_binary_list(<<"section 3">>, <<"key_3">>)),
    ?assertEqual([<<"-100">>], wgconfig:get_binary_list(<<"section 3">>, <<"key_4">>)),
    ?assertEqual([<<"bla bla">>, <<"bla bla bla">>, <<"tata ta ta">>],
                 wgconfig:get_binary_list(<<"section 3">>, <<"key_8">>)),

    ?assertEqual([<<"my binary">>], wgconfig:get_binary_list(<<"section 1">>, <<"key_0">>, [<<"my binary">>])),

    ?assertThrow({wgconfig_error, value_not_found, <<"section 1">>, <<"key_0">>},
                  wgconfig:get_binary_list(<<"section 1">>, <<"key_0">>)),

    application:stop(wgconfig),
    ok.


names_test() ->
    setup(),
    wgconfig:load_configs(["./test/data/my_config3.ini"]),

    ?assertEqual({ok, <<"on">>}, wgconfig:get("section 2", key_3)),

    ?assertEqual(true, wgconfig:get_bool("section 1", <<"key_1">>)),
    ?assertEqual(false, wgconfig:get_bool(<<"section 1">>, "key_2")),

    ?assertEqual(77, wgconfig:get_int('section 1', <<"key_4">>)),
    ?assertEqual(0, wgconfig:get_int(<<"section 3">>, key_5)),

    ?assertEqual([<<"one">>, <<"two">>, <<"three">>, <<"four">>],
                 wgconfig:get_binary_list("section 3", key_1)),

    application:stop(wgconfig),
    ok.


set_test() ->
    setup(),
    wgconfig:load_configs(["./test/data/my_config3.ini"]),

    ?assertEqual(false, wgconfig:get_bool(<<"section 1">>, "key_2")),

    wgconfig:set(<<"section 1">>, <<"key_2">>, true),
    ?assertEqual({ok, <<"true">>}, wgconfig:get(<<"section 1">>, <<"key_2">>)),
    ?assertEqual(true, wgconfig:get_bool(<<"section 1">>, <<"key_2">>)),

    wgconfig:set(<<"section 1">>, <<"key_2">>, 100),
    ?assertEqual({ok, <<"100">>}, wgconfig:get(<<"section 1">>, <<"key_2">>)),
    ?assertEqual(100, wgconfig:get_int(<<"section 1">>, <<"key_2">>)),

    wgconfig:set(<<"section 1">>, <<"key_2">>, 3.14159),
    ?assertEqual(3.14159, wgconfig:get_float(<<"section 1">>, <<"key_2">>)),

    wgconfig:set(<<"section 1">>, <<"key_2">>, "Hello"),
    ?assertEqual({ok, <<"Hello">>}, wgconfig:get(<<"section 1">>, <<"key_2">>)),
    ?assertEqual("Hello", wgconfig:get_string(<<"section 1">>, <<"key_2">>)),

    wgconfig:set(<<"section 1">>, <<"key_2">>, <<"xxx">>),
    ?assertEqual({ok, <<"xxx">>}, wgconfig:get(<<"section 1">>, <<"key_2">>)),
    ?assertEqual(<<"xxx">>, wgconfig:get_binary(<<"section 1">>, <<"key_2">>)),

    application:stop(wgconfig),
    ok.


errors_test() ->
    setup(),
    wgconfig:load_config("./test/data/my_config.ini"),

    ?assertThrow({wgconfig_error, value_not_found, "my_section", "my_key"},
                 wgconfig:get_string("my_section", "my_key")),

    ?assertThrow({wgconfig_error, value_not_found, database, replica},
                 wgconfig:get_binary(database, replica)),

    ?assertThrow({wgconfig_error, value_not_found, database, replica},
                 wgconfig:get_binary(database, replica)),

    ?assertThrow({wgconfig_error, <<"invalid bool 'my_db'">>, database, db_name},
                 wgconfig:get_bool(database, db_name)),

    ?assertThrow({wgconfig_error, <<"invalid int 'my_db'">>, database, db_name},
                 wgconfig:get_int(database, db_name)),

    ?assertThrow({wgconfig_error, <<"invalid float 'my_db'">>, database, db_name},
                 wgconfig:get_float(database, db_name)),

    ok.
