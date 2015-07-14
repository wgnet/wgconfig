-module(wgconfig_storage_tests).

-include_lib("eunit/include/eunit.hrl").

one_config_test() ->
    wgconfig_storage:start_link(),
    {ok, S} = wgconfig_parser:parse_file("../test/my_config.ini"),
    wgconfig_storage:add_sections(S),

    ?assertEqual({ok, <<"localhost">>},
                 wgconfig_storage:get(<<"database">>, <<"host">>)),

    ?assertEqual({ok, <<"5432">>},
                 wgconfig_storage:get(<<"database">>, <<"port">>)),

    ?assertEqual({ok, <<"my_db">>},
                 wgconfig_storage:get(<<"database">>, <<"db_name">>)),

    ?assertEqual({ok, <<"1000">>},
                 wgconfig_storage:get(<<"http_client">>, <<"retry_time">>)),

    ?assertEqual({error,not_found},
                 wgconfig_storage:get(<<"database">>, <<"some_key">>)),

    ?assertEqual({error,not_found},
                 wgconfig_storage:get(<<"some_section">>, <<"port">>)),

    wgconfig_storage:stop(),
    ok.


two_configs_test() ->
    wgconfig_storage:start_link(),
    {ok, S1} = wgconfig_parser:parse_file("../test/my_config.ini"),
    wgconfig_storage:add_sections(S1),

    ?assertEqual({ok, <<"my_db">>},
                 wgconfig_storage:get(<<"database">>, <<"db_name">>)),

    ?assertEqual({ok, <<"my_user">>},
                 wgconfig_storage:get(<<"database">>, <<"db_user">>)),

    ?assertEqual({ok, <<"20">>},
                 wgconfig_storage:get(<<"workers_pool">>, <<"num_workers">>)),

    ?assertEqual({ok, <<"30">>},
                 wgconfig_storage:get(<<"workers_pool">>, <<"max_workers">>)),

    ?assertEqual({error,not_found},
                 wgconfig_storage:get(<<"some_section">>, <<"port">>)),

    {ok, S2} = wgconfig_parser:parse_file("../test/my_config2.ini"),
    wgconfig_storage:add_sections(S2),

    ?assertEqual({ok, <<"test_db">>},
                 wgconfig_storage:get(<<"database">>, <<"db_name">>)),

    ?assertEqual({ok, <<"my_user">>},
                 wgconfig_storage:get(<<"database">>, <<"db_user">>)),

    ?assertEqual({ok, <<"200">>},
                 wgconfig_storage:get(<<"workers_pool">>, <<"num_workers">>)),

    ?assertEqual({ok, <<"300">>},
                 wgconfig_storage:get(<<"workers_pool">>, <<"max_workers">>)),

    ?assertEqual({error,not_found},
                 wgconfig_storage:get(<<"some_section">>, <<"port">>)),

    wgconfig_storage:stop(),
    ok.


names_test() ->
    wgconfig_storage:start_link(),
    {ok, S} = wgconfig_parser:parse_file("../test/my_config.ini"),
    wgconfig_storage:add_sections(S),

    ?assertEqual({ok, <<"localhost">>},
                 wgconfig_storage:get(database, host)),

    ?assertEqual({ok, <<"my_db">>},
                 wgconfig_storage:get("database", "db_name")),

    ?assertEqual({ok, <<"my_db">>},
                 wgconfig_storage:get(<<"database">>, <<"db_name">>)),

    ?assertEqual({ok, <<"1000">>},
                 wgconfig_storage:get(http_client, "retry_time")),

    ?assertEqual({error,not_found},
                 wgconfig_storage:get("database", some_key)),

    ?assertEqual({error,not_found},
                 wgconfig_storage:get(some_section, port)),

    wgconfig_storage:stop(),
    ok.


set_test() ->
    wgconfig_storage:start_link(),
    {ok, S} = wgconfig_parser:parse_file("../test/my_config.ini"),
    wgconfig_storage:add_sections(S),

    ?assertEqual({ok, <<"localhost">>}, wgconfig_storage:get(database, host)),
    wgconfig_storage:set(database, host, <<"127.0.0.1">>),
    ?assertEqual({ok, <<"127.0.0.1">>}, wgconfig_storage:get(database, host)),

    ?assertEqual({ok, <<"my_db">>}, wgconfig_storage:get("database", "db_name")),
    wgconfig_storage:set(database, "db_name", <<"other_db">>),
    ?assertEqual({ok, <<"other_db">>}, wgconfig_storage:get("database", "db_name")),

    ?assertEqual({ok, <<"1000">>}, wgconfig_storage:get(http_client, "retry_time")),
    wgconfig_storage:set(<<"http_client">>, <<"retry_time">>, <<"5000">>),
    ?assertEqual({ok, <<"5000">>}, wgconfig_storage:get(http_client, "retry_time")),

    wgconfig_storage:stop(),
    ok.


list_sections_test() ->
    wgconfig_storage:start_link(),
    ?assertEqual([], wgconfig_storage:list_sections()),

    {ok, S} = wgconfig_parser:parse_file("../test/my_config.ini"),
    wgconfig_storage:add_sections(S),

    L0 = [<<"database">>, <<"http_client">>,
          <<"lager.crash">>, <<"lager.handlers.error">>, <<"lager.handlers.info">>, <<"lager.handlers.warning">>,
          <<"workers_pool">>],
    ?assertEqual(L0, lists:sort(wgconfig_storage:list_sections())),

    L1 = [<<"lager.crash">>, <<"lager.handlers.error">>, <<"lager.handlers.info">>, <<"lager.handlers.warning">>],
    ?assertEqual(L1, lists:sort(wgconfig_storage:list_sections(lager))),
    ?assertEqual(L1, lists:sort(wgconfig_storage:list_sections("lager"))),
    ?assertEqual(L1, lists:sort(wgconfig_storage:list_sections(<<"lager">>))),

    L2 = [<<"lager.handlers.error">>, <<"lager.handlers.info">>, <<"lager.handlers.warning">>],
    ?assertEqual(L2, lists:sort(wgconfig_storage:list_sections("lager.handlers"))),
    ?assertEqual(L2, lists:sort(wgconfig_storage:list_sections(<<"lager.handlers">>))),
    ok.
