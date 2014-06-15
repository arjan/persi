%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Functions relating to schema information

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

-module(persi_schema).


-callback schema_version() ->
    non_neg_integer().

-callback manage(install | {upgrade, non_neg_integer()}, persi:connection()) ->
    ok.


-export(
   [
    info/1,
    table_info/2,
    create_table/2,
    drop_table/2,
    add_column/3,
    drop_column/3,
    manage/2
   ]).

-include_lib("persi/include/persi.hrl").
-include_lib("persi_int.hrl").

-spec info(persi:connection()) -> persi:schema_info().
info(Connection) ->
    Driver = #persi_driver{module=Mod} = persi_connection:lookup_driver(Connection),
    Mod:schema_info(Driver).

-spec table_info(persi:table(), persi:connection()) -> {ok, persi:table_info()} | {error, enotfound}.
table_info(TableName, Connection) ->
    Driver = #persi_driver{module=Mod} = persi_connection:lookup_driver(Connection),
    Mod:table_info(TableName, Driver).

-spec create_table(persi:table_info(), persi:connection()) -> ok | {error, eexist}.
create_table(TableDef, Connection) ->
    case table_info(TableDef#persi_table.name, Connection) of
        {error, enotfound} ->
            Driver = #persi_driver{module=Mod} = persi_connection:lookup_driver(Connection),
            SQL = create_table_sql(TableDef),
            ok = Mod:exec(SQL, Driver),
            ok = Mod:flush_metadata(Driver);
        {ok, #persi_table{}} ->
            {error, eexist}
    end.

-spec drop_table(persi:table(), persi:connection()) -> ok | {error, enotfound}.
drop_table(TableName, Connection) when is_atom(TableName) ->
    case table_info(TableName, Connection) of
        {ok, #persi_table{}} ->
            Driver = #persi_driver{module=Mod} = persi_connection:lookup_driver(Connection),
            ok = Mod:exec([<<"DROP TABLE ">>, atom_to_list(TableName)], Driver),
            ok = Mod:flush_metadata(Driver);
        {error, enotfound} ->
            {error, enotfound}
    end.

-spec add_column(persi:table(), persi:column_info(), persi:connection()) -> ok | {error, enotfound}.
add_column(TableName, ColumnDef, Connection) ->
    case table_info(TableName, Connection) of
        {ok, #persi_table{}} ->
            Driver = #persi_driver{module=Mod} = persi_connection:lookup_driver(Connection),
            ok = Mod:exec([<<"ALTER TABLE ">>, atom_to_list(TableName), <<" ADD COLUMN ">>,
                           create_column_sql(ColumnDef)], Driver),
            ok = Mod:flush_metadata(Driver);
        {error, enotfound} ->
            {error, enotfound}
    end.

-spec drop_column(persi:table(), atom(), persi:connection()) -> ok | {error, enotfound}.
drop_column(TableName, ColumnName, Connection) ->
    case table_info(TableName, Connection) of
        {ok, #persi_table{}} ->
            Driver = #persi_driver{module=Mod} = persi_connection:lookup_driver(Connection),
            ok = Mod:exec([<<"ALTER TABLE ">>, atom_to_list(TableName), <<" DROP COLUMN ">>,
                           atom_to_list(ColumnName)], Driver),
            ok = Mod:flush_metadata(Driver);
        {error, enotfound} ->
            {error, enotfound}
    end.

-spec manage(module(), persi:connection()) -> persi:manage_result().
manage(SchemaModule, Connection) ->
    case table_info(schema_version, Connection) of
        {error, enotfound} ->
            ok = create_table(
                   #persi_table{name=schema_version,
                                columns=[
                                         #persi_column{name=schema, type="varchar(255)", notnull=true},
                                         #persi_column{name=version, type=int, notnull=true, default=1}
                                        ],
                                pk=[schema]}, Connection);
        {ok, _} ->
            nop
    end,
    Version = SchemaModule:schema_version(),
    {ok, SchemaResult} = persi:fetchall(<<"SELECT version FROM schema_version WHERE schema = ?">>, [SchemaModule], Connection),
    case SchemaResult of
        {[], _, _} ->
            %% install
            ok = SchemaModule:manage(install, Connection),
            %% insert version
            persi:fetchall(<<"INSERT INTO schema_version (schema, version) VALUES (?, ?)">>, [SchemaModule, Version], Connection),
            install;
        {[{Version}], _, _} ->
            noop;
        {[{OlderVersion}], _, _} when OlderVersion < Version ->
            Upgrades = lists:seq(OlderVersion+1, Version),
            [ok = SchemaModule:manage({upgrade, V}, Connection) || V <- Upgrades],
            persi:fetchall(<<"UPDATE schema_version SET version = ? WHERE schema = ?">>, [Version, SchemaModule], Connection),
            {upgrade, Version};
        {[{_}], _, _} ->
            throw({error, schema_downgrade})
    end.


create_table_sql(#persi_table{name=Name, columns=Columns, pk=PK, fks=FKs}) ->
    ["CREATE TABLE ", atom_to_list(Name), " (",
     persi_util:iolist_join(
       [create_column_sql(C) || C <- Columns]
       ++ primary_key_sql(PK)
       ++ [foreign_key_sql(FK) || FK <- FKs],
       $,
      ),
     ")"].

create_column_sql(#persi_column{name=Name, type=Type, default=Default, notnull=Notnull}) ->
    [atom_to_list(Name),
     " ", map_sql_type(Type),
     " ", map_sql_default(Default),
     " ", map_sql_notnull(Notnull)
    ].

map_sql_type(T) when is_binary(T); is_list(T) ->
    T;
map_sql_type(T) when is_atom(T) ->
    atom_to_list(T).

map_sql_default(undefined) ->
    "";
map_sql_default(T) when is_integer(T) ->
    map_sql_default(integer_to_list(T));
map_sql_default(X) when is_binary(X); is_list(X) ->
    ["DEFAULT '", X, "'"].

map_sql_notnull(true) ->
    "NOT NULL";
map_sql_notnull(false) ->
    "NULL".


primary_key_sql([]) ->
    [];
primary_key_sql(Cols) ->
    [["PRIMARY KEY (", persi_util:iolist_join(lists:map(fun atom_to_list/1, Cols), $,), ")"]].

foreign_key_sql(#persi_fk{table=Table, from=From, to=To}) ->
    ["FOREIGN KEY (", atom_to_list(From) ,") REFERENCES ", atom_to_list(Table), "(", atom_to_list(To), ")"].

