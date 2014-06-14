-module(persi_schema_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

create_table_test() ->

    ok = application:start(persi),

    persi:add_connection(default, [{driver, persi_driver_esqlite}, {dbfile, "/tmp/test.db"}]),
    ?assertEqual(1,2).
