
setup() ->
    application:start(gproc),
    application:start(persi),

    DriverOpts = case os:getenv("PERSI_DB") of
                     _ ->
                         [{driver, persi_driver_esqlite}, {dbfile, ":memory:"}]
                 end,
    ok = persi:add_connection(DriverOpts),    
    ok.

teardown() ->
    %% application:stop(persi),
    %% application:stop(gproc),
    persi:remove_connection(),
    ok.
    
