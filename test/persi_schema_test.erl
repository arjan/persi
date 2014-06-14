-module(persi_schema_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

dup_connection_test() ->

    ok = application:start(gproc),
    ok = application:start(persi),

    ok = persi:add_connection(default, [{driver, persi_driver_esqlite}, {dbfile, "/tmp/test.db"}]),
    {error, eexist} = persi:add_connection(default, []),
    
    ok.
