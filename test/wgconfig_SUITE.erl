-module(wgconfig_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([all/0,
         init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
         reload_test/1, subscribe_test/1
        ]).


-define(CONFIG_FILES, ["./test/data/my_config.ini", "./test/data/my_config2.ini"]).


all() ->
    [reload_test,
     subscribe_test
    ].


init_per_suite(Config) ->
    application:ensure_all_started(wgconfig),
    Config.


end_per_suite(Config) ->
    application:stop(wgconfig),
    Config.


init_per_testcase(_, Config) ->
    file:set_cwd(code:lib_dir(wgconfig)),
    Res = wgconfig:load_configs(?CONFIG_FILES),
    ct:pal("load_configs ~p res ~p", [?CONFIG_FILES, Res]),
    Config.


end_per_testcase(_, Config) ->
    Config.


reload_test(_Config) ->
    ?assertEqual(<<"test_db">>, wgconfig:get_binary(<<"database">>, <<"db_name">>)),
    ?assertEqual("my_user", wgconfig:get_string(<<"database">>, <<"db_user">>)),
    ?assertEqual(200, wgconfig:get_int(<<"workers_pool">>, <<"num_workers">>)),

    ?assertEqual(?CONFIG_FILES, wgconfig_storage:get_config_files()),
    ?assertEqual([ok, ok], wgconfig:reload()),

    ?assertEqual(<<"test_db">>, wgconfig:get_binary(<<"database">>, <<"db_name">>)),
    ?assertEqual("my_user", wgconfig:get_string(<<"database">>, <<"db_user">>)),
    ?assertEqual(200, wgconfig:get_int(<<"workers_pool">>, <<"num_workers">>)),

    CF = hd(?CONFIG_FILES),
    wgconfig_storage:save_config_files([CF]),
    ?assertEqual([CF], wgconfig_storage:get_config_files()),
    ?assertEqual([ok], wgconfig:reload()),
    ?assertEqual(<<"my_db">>, wgconfig:get_binary(<<"database">>, <<"db_name">>)),

    ok.


subscribe_test(_Config) ->
    wgconfig:subscribe(sample_handler),

    Events1 = gen_event:call(wgconfig_event_manager, sample_handler, get_state),
    ct:pal("Events1: ~p", [Events1]),
    ?assertEqual([], Events1),

    wgconfig:reload(),

    Events2 = gen_event:call(wgconfig_event_manager, sample_handler, get_state),
    ct:pal("Events2: ~p", [Events2]),
    ?assertEqual([wgconfig_reload], Events2),

    ok.
