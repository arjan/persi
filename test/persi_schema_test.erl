-module(persi_schema_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

dup_default_connection_test() ->
    application:start(gproc),
    application:start(persi),

    ok = persi:add_connection([{driver, persi_driver_esqlite}, {dbfile, "/tmp/test.db"}]),
    {error, eexist} = persi:add_connection([]),
    ok.

dup_named_connection_test() ->
    application:start(gproc),
    application:start(persi),

    ok = persi:add_connection(sqlite1, [{driver, persi_driver_esqlite}, {dbfile, "/tmp/test.db"}]),
    ok = persi:add_connection(sqlite2, [{driver, persi_driver_esqlite}, {dbfile, "/tmp/test1.db"}]),
    
    {error, eexist} = persi:add_connection(sqlite1, []),
    ok.


schema_info_test() ->
    application:start(gproc),
    application:start(persi),

    persi:add_connection([{driver, persi_driver_esqlite}, {dbfile, "/tmp/test.db"}]),

    Info = persi:schema_info(),
    io:format(user, "~p~n", [Info]),

    TableInfo = persi:table_info(db_version),
    io:format(user, "~p~n", [TableInfo]),
    ok.

