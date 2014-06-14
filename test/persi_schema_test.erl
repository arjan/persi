-module(persi_schema_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("persi/include/persi.hrl").

-define(DBFILE, ":memory:").

setup() ->
    application:start(gproc),
    application:start(persi),
    ok.

dup_default_connection_test() ->
    application:start(gproc),
    application:start(persi),

    ok = persi:add_connection([{driver, persi_driver_esqlite}, {dbfile, ?DBFILE}]),
    {error, eexist} = persi:add_connection([]),
    ok.

dup_named_connection_test() ->
    application:start(gproc),
    application:start(persi),

    ok = persi:add_connection(sqlite1, [{driver, persi_driver_esqlite}, {dbfile, ?DBFILE}]),
    ok = persi:add_connection(sqlite2, [{driver, persi_driver_esqlite}, {dbfile, "/tmp/test1.db"}]),
    
    {error, eexist} = persi:add_connection(sqlite1, []),
    ok.

schema_info_test() ->
    application:start(gproc),
    application:start(persi),
    persi:add_connection([{driver, persi_driver_esqlite}, {dbfile, ?DBFILE}]),
    #persi_schema{} = persi:schema_info(),
    ok.

table_info_test() ->
    application:start(gproc),
    application:start(persi),
    persi:add_connection([{driver, persi_driver_esqlite}, {dbfile, ?DBFILE}]),
    {error, enotfound} = persi:table_info(fjdlkfjdslkfjdslkfjdslkjflkds),
    ok.

create_table_test() ->
    setup(),
    persi:add_connection([{driver, persi_driver_esqlite}, {dbfile, ?DBFILE}]),

    Table = #persi_table{name=hello,
                         columns=
                             [
                              #persi_column{name=id, type=int},
                              #persi_column{name=name, type="varchar(50)", default="app", notnull=true}
                             ],
                        pk=[id]},
    
    ok = persi:create_table(Table),
    {error, eexist} = persi:create_table(Table),
    ok.

schema_migration_test() ->
    persi:add_connection([{driver, persi_driver_esqlite}, {dbfile, ?DBFILE}]),

    install = persi:manage_schema(schema_migrations_example),
    noop = persi:manage_schema(schema_migrations_example),
    
    ok.
