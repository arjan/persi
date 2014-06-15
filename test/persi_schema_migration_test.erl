%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Test schema migration tests

%% Copyright 2014 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(persi_schema_migration_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("persi/include/persi.hrl").

-include("persi_eunit.hrl").
    
schema_migration_test() ->
    setup(),

    meck:new(migrations_example, [non_strict]),

    meck:expect(
      migrations_example, manage,
      fun(install, Connection) ->
              persi:create_table(#persi_table{name=test, columns=[#persi_column{name=id, type=int, notnull=true}], pk=[id]}, Connection);
         ({upgrade, 2}, Connection) ->
              {error, enotfound} = persi:add_column(jfdslkfjlkdsfjlkdsjf, #persi_column{name=name, type="varchar(60)"}, Connection),

              ok = persi:add_column(test, #persi_column{name=name, type="varchar(60)"}, Connection),
              {ok, T} = persi:table_info(test, Connection),
              [id, name] = [C#persi_column.name || C <- T#persi_table.columns],
              ok;
         ({upgrade, 3}, Connection) ->
              ok = persi:add_column(test, #persi_column{name=lastname, type="varchar(120)"}, Connection),
              {ok, T} = persi:table_info(test, Connection),
              [id, name, lastname] = [C#persi_column.name || C <- T#persi_table.columns],
              ok;
         ({upgrade, 4}, Connection) ->
              %% DROP COLUMN not supported by sqlite3
              %%ok = persi:drop_column(test, name, Connection),

              ok = persi:drop_table(test, Connection),
              {error, enotfound} = persi:drop_table(test, Connection),
              {error, enotfound} = persi:table_info(test, Connection),
              ok
      end
     ),
    
    meck:expect(migrations_example, schema_version, fun() -> 1 end),
    install = persi:manage_schema(migrations_example),
    noop = persi:manage_schema(migrations_example),

    meck:expect(migrations_example, schema_version, fun() -> 4 end),
    {upgrade, 4} = persi:manage_schema(migrations_example),
    noop = persi:manage_schema(migrations_example),

    teardown().
