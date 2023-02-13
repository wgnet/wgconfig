-type(wgconfig_name() :: binary() | string() | atom()).

-type(wgconfig_section() :: binary()).
-type(wgconfig_key() :: binary()).
-type(wgconfig_value() :: binary()).

-type(parsed_ini_config() :: #{{wgconfig_section(), wgconfig_key()} => wgconfig_value()}).
-type(wgconfig() :: #{wgconfig_section() => #{wgconfig_key() => wgconfig_value()}}).
