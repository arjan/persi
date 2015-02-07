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
    manage/2,
    flush_metadata/1
   ]).

-include_lib("persi/include/persi.hrl").
-include_lib("persi_int.hrl").

-define(param(N), Mod:map_dialect({sql_parameter, N})).

-spec info(persi:connection() | #persi_driver{}) -> persi:schema_info().
info(Connection) when is_atom(Connection) ->
    info(persi_connection:lookup_driver(Connection));
info(Driver = #persi_driver{module=Mod}) ->
    Mod:schema_info(Driver).

-spec table_info(persi:table(), persi:connection() | #persi_driver{}) -> {ok, persi:table_info()} | {error, enotfound}.
table_info(TableName, Connection) when is_atom(Connection) ->
    table_info(TableName, persi_connection:lookup_driver(Connection));
table_info(TableName, Driver = #persi_driver{module=Mod}) ->
    Mod:table_info(TableName, Driver).

-spec create_table(persi:table_info(), persi:connection() | #persi_driver{}) -> ok | {error, eexist}.
create_table(TableDef, Connection) when is_atom(Connection) ->
    create_table(TableDef, persi_connection:lookup_driver(Connection));
create_table(TableDef, Driver = #persi_driver{module=Mod}) ->
    case table_info(TableDef#persi_table.name, Driver) of
        {error, enotfound} ->
            SQL = create_table_sql(TableDef, Mod),
            ok = Mod:exec(SQL, Driver),
            ok = Mod:flush_metadata(Driver);
        {ok, #persi_table{}} ->
            {error, eexist}
    end.

-spec drop_table(persi:table(), persi:connection() | #persi_driver{}) -> ok | {error, enotfound}.
drop_table(TableName, Connection) when is_atom(TableName), is_atom(Connection) ->
    drop_table(TableName, persi_connection:lookup_driver(Connection));
drop_table(TableName, Driver = #persi_driver{module=Mod}) ->
    case table_info(TableName, Driver) of
        {ok, #persi_table{}} ->
            T = Mod:map_dialect({quote_literal, atom_to_list(TableName)}),
            ok = Mod:exec([<<"DROP TABLE ">>, T], Driver),
            ok = Mod:flush_metadata(Driver);
        {error, enotfound} ->
            {error, enotfound}
    end.

-spec add_column(persi:table(), persi:column_info(), persi:connection() | #persi_driver{}) -> ok | {error, enotfound}.
add_column(TableName, ColumnDef, Connection) when is_atom(Connection) ->
    add_column(TableName, ColumnDef, persi_connection:lookup_driver(Connection));
add_column(TableName, ColumnDef, Driver = #persi_driver{module=Mod}) ->
    case table_info(TableName, Driver) of
        {ok, #persi_table{has_props=HasProps}} ->
            true = Mod:map_dialect({check_support, add_column}),
            T = Mod:map_dialect({quote_literal, atom_to_list(TableName)}),
            ok = Mod:exec([<<"ALTER TABLE ">>, T, <<" ADD COLUMN ">>,
                           create_column_sql(ColumnDef, Mod)], Driver),
            case HasProps of
                true ->
                    migrate_from_props(TableName, ColumnDef#persi_column.name, Driver);
                false ->
                    nop
            end,
            ok = Mod:flush_metadata(Driver);
        {error, enotfound} ->
            {error, enotfound}
    end.

-spec drop_column(persi:table(), atom(), persi:connection() | #persi_driver{}) -> ok | {error, enotfound}.
drop_column(TableName, ColumnName, Connection) when is_atom(Connection) ->
    drop_column(TableName, ColumnName, persi_connection:lookup_driver(Connection));
drop_column(TableName, ColumnName, Driver = #persi_driver{module=Mod}) ->
    case table_info(TableName, Driver) of
        {ok, #persi_table{has_props=HasProps}} ->
            true = Mod:map_dialect({check_support, drop_column}),
            case HasProps of
                true ->
                    migrate_to_props(TableName, ColumnName, Driver);
                false ->
                    nop
            end,
            T = Mod:map_dialect({quote_literal, atom_to_list(TableName)}),
            ok = Mod:exec([<<"ALTER TABLE ">>, T, <<" DROP COLUMN ">>,
                           atom_to_list(ColumnName)], Driver),
            ok = Mod:flush_metadata(Driver);
        {error, enotfound} ->
            {error, enotfound}
    end.

-spec manage(module(), persi:connection() | #persi_driver{}) -> persi:manage_result().
manage(SchemaModule, Connection) when is_atom(Connection) ->
    manage(SchemaModule, persi_connection:lookup_driver(Connection));
manage(SchemaModule, Driver = #persi_driver{module=Mod}) ->
    case table_info(schema_version, Driver) of
        {error, enotfound} ->
            ok = create_table(
                   #persi_table{name=schema_version,
                                columns=[
                                         #persi_column{name=schema_module, type="varchar(255)", notnull=true},
                                         #persi_column{name=version, type=int, notnull=true, default=1}
                                        ],
                                pk=[schema_module]}, Driver);
        {ok, _} ->
            nop
    end,
    Version = SchemaModule:schema_version(),
    {ok, SchemaResult} = persi_query:q(["SELECT version FROM schema_version WHERE schema_module = ", ?param(1)], [SchemaModule], Driver),
    case SchemaResult of
        {[], _, _} ->
            %% install
            ok = SchemaModule:manage(install, Driver),
            %% insert version
            persi_query:q(["INSERT INTO schema_version (schema_module, version) VALUES (", ?param(1), ", ", ?param(2), ")"],
                                 [SchemaModule, Version], Driver),
            install;
        {[[Version]], _, _} ->
            noop;
        {[[OlderVersion]], _, _} when OlderVersion < Version ->
            Upgrades = lists:seq(OlderVersion+1, Version),
            [ok = SchemaModule:manage({upgrade, V}, Driver) || V <- Upgrades],
            persi_query:q(["UPDATE schema_version SET version = ", ?param(1), " WHERE schema_module = ", ?param(2)],
                                 [Version, SchemaModule], Driver),
            {upgrade, Version};
        {[[_]], _, _} ->
            throw({error, schema_downgrade})
    end.

-spec flush_metadata(persi:connection() | #persi_driver{}) -> ok.
flush_metadata(Connection) when is_atom(Connection) ->
    flush_metadata(persi_connection:lookup_driver(Connection));
flush_metadata(Driver = #persi_driver{module=Mod}) ->
    ok = Mod:flush_metadata(Driver).


create_table_sql(#persi_table{name=TableName, columns=Columns, pk=PK, fks=FKs}, DriverModule) ->
    T = DriverModule:map_dialect({quote_literal, atom_to_list(TableName)}),
    ["CREATE TABLE ", T, " (",
     persi_util:iolist_join(
       [create_column_sql(C, DriverModule) || C <- Columns]
       ++ primary_key_sql(PK, DriverModule)
       ++ [foreign_key_sql(FK, DriverModule) || FK <- FKs],
       $,
      ),
     ")"].

create_column_sql(Column=#persi_column{name=Name, default=Default, notnull=Notnull}, DriverModule) ->
    true = DriverModule:map_dialect({check_support, Column}),
    C = DriverModule:map_dialect({quote_literal, atom_to_list(Name)}),
    [C,
     " ", map_sql_type(Column, DriverModule),
     " ", map_sql_default(Default, DriverModule),
     " ", map_sql_notnull(Notnull, DriverModule)
    ].

map_sql_type(#persi_column{type=T}, _) when is_binary(T); is_list(T) ->
    T;
map_sql_type(Column=#persi_column{type=T}, DriverModule) when is_atom(T) ->
    case persi_util:map_column_to_sql(Column) of
        undefined ->
            case DriverModule:map_dialect({columntype, Column}) of
                A when is_atom(A) -> atom_to_list(A);
                B -> B
            end;
        R -> R
    end.


map_sql_default(undefined, _DriverModule) ->
    "";
map_sql_default(T, _DriverModule) when is_integer(T) ->
    map_sql_default(integer_to_list(T), _DriverModule);
map_sql_default(B, _DriverModule) when B =:= true; B =:= false ->
    ["DEFAULT ", atom_to_list(B)];
map_sql_default(current_timestamp, _DriverModule) ->
    ["DEFAULT CURRENT_TIMESTAMP"];
map_sql_default(X, _DriverModule) when is_binary(X); is_list(X) ->
    ["DEFAULT '", X, "'"].

map_sql_notnull(true, _DriverModule) ->
    "NOT NULL";
map_sql_notnull(false, _DriverModule) ->
    "NULL".


primary_key_sql([], _DriverModule) ->
    [];
primary_key_sql(Cols, _DriverModule) ->
    [["PRIMARY KEY (", persi_util:iolist_join(lists:map(fun atom_to_list/1, Cols), $,), ")"]].

foreign_key_sql(#persi_fk{table=TableName, from=From, to=To}, DriverModule) ->
    T = DriverModule:map_dialect({quote_literal, atom_to_list(TableName)}),
    C = DriverModule:map_dialect({quote_literal, atom_to_list(To)}),
    ["FOREIGN KEY (", atom_to_list(From) ,") REFERENCES ", T, "(", C, ")"].



%% @doc Get a value from the props and put it in its own column (as a first-class citizen of the table)
%% FIXME wrap this in a single transaction instead of making separate calls to the driver
migrate_from_props(TableName, ColumnName, #persi_driver{module=Mod}=Driver) ->
    Mod:exec("BEGIN", Driver),
    T = Mod:map_dialect({quote_literal, atom_to_list(TableName)}),
    {ok, {All, _, _}} = Mod:q(["SELECT id, props FROM ", T], [], Driver),
    [begin
         Props = binary_to_term(PropsBin),
         case proplists:lookup(ColumnName, Props) of
             none ->
                 skip;
             {_, V} -> 
                 C = Mod:map_dialect({quote_literal, atom_to_list(ColumnName)}),
                 Mod:q(["UPDATE ", T, " SET ",
                               C, " = ", ?param(1),
                               ", props = ", ?param(2), " WHERE id = ", ?param(3)],
                              [V, term_to_binary(proplists:delete(ColumnName, Props)), Id],
                              Driver)
         end
     end || [Id, PropsBin] <- All
    ],
    ok = Mod:exec("COMMIT", Driver),
    {ok, length(All)}.

migrate_to_props(TableName, ColumnName, #persi_driver{module=Mod}=Driver) ->
    Mod:exec("BEGIN", Driver),
    T = Mod:map_dialect({quote_literal, atom_to_list(TableName)}),
    C = Mod:map_dialect({quote_literal, atom_to_list(ColumnName)}),
    {ok, {All, _, _}} = Mod:q(["SELECT id, ", C, ", props FROM ", T], [], Driver),
    [begin
         case Value of
             undefined ->
                 skip;
             _ -> 
                 Props = binary_to_term(PropsBin),
                 NewProps = [{ColumnName, Value} | proplists:delete(ColumnName, Props)],
                 Mod:q(["UPDATE ", T,
                               " SET props = ", ?param(1), " WHERE id = ", ?param(2)],
                              [term_to_binary(NewProps), Id],
                              Driver)
         end
     end || [Id, Value, PropsBin] <- All
    ],
    ok = Mod:exec("COMMIT", Driver),
    {ok, length(All)}.


%% merge_column(A, B) ->
%%     list_to_tuple(
%%       lists:map(fun({undefined, X}) -> X;
%%                    ({X, _}) -> X end,
%%                 lists:zip(tuple_to_list(A), tuple_to_list(B))
%%                )).
                         
