# wgconfig
[![Build Status][gh badge]][gh]
[![Erlang Versions][erlang version badge]][gh]

**wgconfig** provides ini config files support.

Create configuration files:
```toml
# My cool config

[database]
host = "localhost"
port = 5432
db_name = "my_db"
db_user = "my_user"

[workers_pool]
num_workers = 20 # number of workers
max_workers = 30

[http_client]
http_timeout = 5000
retry_time = 1000
max_retries = 7
```

Hash symbol _#_ is used for comments. If you need string value which
containing hash inside it, you should quote it with single or double
quotes.

```toml
some_key = "string with # inside it" # comment
other_key = 'another string with # inside it' # comment
next_key = "string with single quote ' inside it"
yet_another_key = 'string with double quote " inside it'
```

Start app and load config files:

```erlang
application:start(wgconfig),
wgconfig:load_configs(["path/to/my_config.ini", "path/to/my_other_config.ini"]),
```

Read values from config:

```erlang
wgconfig:get_string(database, host), % "localhost"
wgconfig:get_int(database, port, 5432), % 5432
wgconfig:get_binary(database, db_name), % <<"my_db">>
wgconfig:get_binary(database, db_user), % <<"my_user">>
```

See [Unit Tests](./test/wgconfig_tests.erl) for more examples.


## Reload config and process changes

To reload config files call

```erlang
wgconfig:reload().
```

If you need to be notified and process changes
create module with **gen_event** behaviour

```erlang
-module(my_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2]).

init(_Args) ->
    {ok, []}.

handle_event(Event, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.
```

subscribe to get notifications

```erlang
wgconfig:subscribe(my_handler).
```

and process **wgconfig_reload** event.

```erlang
handle_event(wgconfig_reload, State) ->
    %% process this event
    {ok, State}.
```

<!-- Badges -->
[gh]: https://github.com/wgnet/wgconfig/actions/workflows/ci.yml
[gh badge]: https://img.shields.io/github/workflow/status/wgnet/wgconfig/CI?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-21.3%20to%2024.2-blue.svg?style=flat-square
