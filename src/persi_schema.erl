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

-export(
   [
    info/1,
    table_info/2,
    create_table/2
   ]).

-include_lib("persi/include/persi.hrl").

-spec info(persi:connection()) -> persi:schema_info().
info(Connection) ->
    {Mod, Pid} = persi_connection:driver_and_pid(Connection),
    Mod:schema_info(Pid).

-spec table_info(persi:table(), persi:connection()) -> persi:table_info().
table_info(TableName, Connection) ->
    {Mod, Pid} = persi_connection:driver_and_pid(Connection),
    Mod:table_info(TableName,Pid).


-spec create_table(persi:table_info(), persi:connection()) -> ok | {error, eexist}.
create_table(TableDef, Connection) ->
    case table_info(TableDef#persi_table.name, Connection) of
        {error, enotfound} ->
            {Mod, Pid} = persi_connection:driver_and_pid(Connection),
            SQL = create_table_sql(TableDef),
            Mod:exec(SQL, Pid),
            ok;
        #persi_table{} ->
            {error, eexist}
    end.

with_commas([]) -> [];
with_commas([X]) -> [X];
with_commas([First|Rest]) ->
    lists:reverse(with_commas(Rest, [First])).
with_commas([], Acc) ->
    Acc;
with_commas([H|T], Acc) ->
    with_commas(T, [H,$,|Acc]).


create_table_sql(#persi_table{name=Name, columns=Columns, pk=PK, fks=FKs}) ->
    ["CREATE TABLE ", atom_to_list(Name), " (",
       with_commas(
         [create_column_sql(C) || C <- Columns]
         ++ primary_key_sql(PK)
         ++ [foreign_key_sql(FK) || FK <- FKs]
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
map_sql_default(X) when is_binary(X); is_list(X) ->
    ["DEFAULT '", X, "'"].

map_sql_notnull(true) ->
    "NOT NULL";
map_sql_notnull(false) ->
    "NULL".


primary_key_sql([]) ->
    [];
primary_key_sql(Cols) ->
    [["PRIMARY KEY (", with_commas(lists:map(fun atom_to_list/1, Cols)), ")"]].

foreign_key_sql(#persi_fk{table=Table, from=From, to=To}) ->
    ["FOREIGN KEY ", atom_to_list(From) ," REFERENCES ", atom_to_list(Table), "(", atom_to_list(To), ")"].

