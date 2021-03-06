start_persi() ->
    application:start(gproc),
    application:start(persi),
    ok.
    
setup() ->
    ok = start_persi(),
    {Mod, Opts} = case os:getenv("DBDRIVER") of
                     "mysql" ->
                         crypto:start(),
                         application:start(emysql),
                          {persi_driver_emysql, [{user, "root"}, {password, ""}, {database, "persi_test"}]};
                     "pgsql" ->
                         application:start(epgsql),
                          {persi_driver_epgsql, [{user, "persi"}, {password, ""}, {database, "persi_test"}]};
                     _ ->
                          {persi_driver_esqlite, [{dbfile, ":memory:"}]}
                 end,
    ok = persi:add_connection(Mod, Opts),    
    ok.

teardown() ->
    %% application:stop(persi),
    %% application:stop(gproc),

    %% clean up
    I = persi:schema_info(),
    [persi:drop_table(T#persi_table.name) || T <- lists:reverse(I#persi_schema.tables)],

     persi:remove_connection(),
    ok.
    
