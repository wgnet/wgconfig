**wgconfig** provides ini config files support.

Create configuration files:
```
    # My cool config

    [database]
    host = localhost
    port = 5432
    db_name = my_db
    db_user = my_user

    [workers_pool]
    num_workers = 20 # number of workers
    max_workers = 30

    [http_client]
    http_timeout = 5000
    retry_time = 1000
    max_retries = 7
```

Start app and load config files:

```
    application:start(wgconfig),
    wgconfig:load_configs(["path/to/my_config.ini", "path/to/my_other_config.ini"]),
```

Read values from config:

```
    wgconfig:get_string(database, host), % "localhost"
    wgconfig:get_int(database, port, 5432), % 5432
    wgconfig:get_binary(database, db_name), % <<"my_db">>
    wgconfig:get_binary(database, db_user), % <<"my_user">>
```

See unit tests for more examples.
