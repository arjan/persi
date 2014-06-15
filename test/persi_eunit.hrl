
setup() ->
    application:start(gproc),
    application:start(persi),

    DriverOpts = case os:getenv("PERSI_DBDRIVER") of
                     "mysql" ->

                         crypto:start(),
                         application:start(emysql),
                         
                         [{driver, persi_driver_emysql}, {user, "root"}, {password, "nfgcoal"}, {database, "testdb"}];
                     
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
    
