%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Operations on a single table

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

-module(persi_table).

-include_lib("persi.hrl").

-export(
   [
    insert/3,
    update/4,
    upsert/4,
    delete/3,
    select/3
   ]).


-spec insert(persi:table(), persi:row(), persi:connection()) -> {ok, persi:id()} | persi:error().
insert(TableName, Row, Connection) when is_atom(TableName) ->
    {Mod, Pid} = persi_connection:driver_and_pid(Connection),

    {Cols, Args} = lists:foldr(
                     fun({K, V}, {C0, A0}) ->
                             {[atom_to_list(K)|C0], [V|A0]}
                     end,
                     {[], []},
                     Row),
    
    Sql = [<<"INSERT INTO ">>, atom_to_list(TableName),
           " (",
           persi_util:iolist_join(Cols, $,),
           ") VALUES (",
           persi_util:iolist_join([$? || _ <- lists:seq(1, length(Cols))], $,),
           ")"],
    case Mod:fetchall(Sql, Args, Pid) of
        {ok, _} ->
            ok;
        {error, _} = E ->
            E
    end.


update(_, _, _, _) ->
    ok.

upsert(_, _, _, _) ->
    ok.

-spec delete(persi:table(), persi:selection(), persi:connection()) -> ok | persi:error().
delete(TableName, Selection, Connection) when is_atom(TableName) ->
    {Mod, Pid} = persi_connection:driver_and_pid(Connection),

    {Where, Args} = selection_where(Selection),
    Sql = [<<"DELETE FROM ">>, atom_to_list(TableName), " WHERE ", Where],
    case Mod:fetchall(Sql, Args, Pid) of
        {ok, {_, _, 0}} ->
            {error, enotfound};
        {ok, {_, _, Nr}} ->
            {ok, Nr};
        {error, _} = E ->
            E
    end.

-spec select(persi:table(), persi:selection(), persi:connection()) -> {ok, persi:row()} | persi:error().
select(TableName, Selection, Connection) when is_atom(TableName) ->
    {Mod, Pid} = persi_connection:driver_and_pid(Connection),

    {Where, Args} = selection_where(Selection),
    Sql = [<<"SELECT * FROM ">>, atom_to_list(TableName), " WHERE ", Where, " LIMIT 1"],

    case Mod:fetchall(Sql, Args, Pid) of
        {ok, {[], _, _}} ->
            {error, enotfound};
        {ok, {[Values], Columns, _}} ->
            Row = lists:zip(tuple_to_list(Columns), tuple_to_list(Values)),
            {ok, Row};
        {error, _} = E ->
            E
    end.


-spec selection_where(persi:selection()) -> {WhereClause::binary(), persi:sql_args()}.
selection_where(Simple) when is_binary(Simple);
                             is_integer(Simple);
                             is_atom(Simple)->
    selection_where([{id, Simple}]);

selection_where([]) ->
    throw({error, empty_selection});
selection_where(KVs) when is_list(KVs) ->
    {Clauses, Args} = lists:foldr(
                        fun({K, V}, {C0, A0}) ->
                                {[ [atom_to_list(K), " = ?"] | C0], [V | A0]}
                        end,
                        {[], []},
                        KVs),
    {persi_util:iolist_join(Clauses, " AND "), Args}.

                                   
