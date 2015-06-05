-type(wgconfig_name() :: binary() | string() | atom()).

-type(wgconfig_key_value() :: {binary(), binary()}).
-type(wgconfig_section_name() :: binary()).
-type(wgconfig_section() :: {wgconfig_section_name(), [wgconfig_key_value()]}).
