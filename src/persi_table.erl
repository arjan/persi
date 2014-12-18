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

-include_lib("persi/include/persi.hrl").
-include("persi_int.hrl").

-export(
   [
    insert/3,
    update/4,
    upsert/4,
    delete/3,
    select/3
   ]).

-define(param(N), Mod:map_dialect({sql_parameter, N})).


-spec insert(persi:table(), persi:row(), persi:connection() | #persi_driver{}) -> ok | persi:error().
insert(TableName, Row0, Connection) when is_atom(TableName), is_atom(Connection) ->
    insert(TableName, Row0, persi_connection:lookup_driver(Connection));
insert(TableName, Row0, Driver = #persi_driver{module=Mod}) ->

    {ok, TableInfo} = persi_schema:table_info(TableName, Driver),
    Row = opt_fold_props(TableInfo, Row0, undefined, Driver),
    Types = [{C#persi_column.name, C#persi_column.type} || C <- TableInfo#persi_table.columns],
    
    {Cols, Args} = lists:foldr(
                     fun({K, V}, {C0, A0}) ->
                             Value = Mod:map_dialect({columnvalue, proplists:get_value(K, Types), V}),
                             Col = atom_to_list(K),
                             {[Col|C0], [Value|A0]}
                     end,
                     {[], []},
                     Row),
    
    Sql = [<<"INSERT INTO ">>, atom_to_list(TableName),
           " (",
           persi_util:iolist_join(Cols, $,),
           ") VALUES (",
           persi_util:iolist_join([?param(N) || N <- lists:seq(1, length(Cols))], $,),
           ")"],

    case Mod:q(Sql, Args, Driver) of
        {ok, {[[1]], _, _}} ->
            ok;
        {error, _} = E ->
            E
    end.


-spec update(persi:table(), persi:selection(), persi:row(), persi:connection() | #persi_driver{}) -> {ok, non_neg_integer()} | persi:error().
update(TableName, Selection, Row0, Connection) when is_atom(TableName), is_atom(Connection) ->
    update(TableName, Selection, Row0, persi_connection:lookup_driver(Connection));
update(TableName, Selection, Row0, Driver = #persi_driver{module=Mod}) ->

    {ok, TableInfo} = persi_schema:table_info(TableName, Driver),
    Row = opt_fold_props(TableInfo, Row0, Selection, Driver),
    Types = [{C#persi_column.name, C#persi_column.type} || C <- TableInfo#persi_table.columns],

    case Row of
        [] ->
            {error, nodata};
        _ ->
            {Ks,_} = lists:unzip(Row),
            Vs1 = lists:foldr(
                    fun({K, V}, Acc) ->
                            Value = Mod:map_dialect({columnvalue, proplists:get_value(K, Types), V}),
                            [Value|Acc]
                    end,
                    [],
                    Row),

            Ksn = lists:zip(lists:seq(1, length(Ks)), Ks),
            Sets = persi_util:iolist_join(
                     [[atom_to_list(K), " = ", ?param(N)] || {N, K} <- Ksn], $,),

            {Where, WhereArgs} = selection_where(Selection, Mod, length(Vs1)+1),
            Sql = [<<"UPDATE ">>, atom_to_list(TableName), " SET ", Sets, " WHERE ", Where],

            case Mod:q(Sql, Vs1 ++ WhereArgs, Driver) of
                {ok, {[[0]], _, _}} ->
                    {error, enotfound};
                {ok, {[[Nr]], _, _}} ->
                    {ok, Nr};
                {error, _} = E ->
                    E
            end
    end.

-spec upsert(persi:table(), persi:selection(), persi:row(), persi:connection() | #persi_driver{}) -> {ok, non_neg_integer()} | persi:error().
upsert(TableName, Selection, Row, Connection) when is_atom(TableName), is_atom(Connection) ->
    upsert(TableName, Selection, Row, persi_connection:lookup_driver(Connection));
upsert(TableName, Selection, Row, Driver = #persi_driver{}) ->
    case select(TableName, Selection, Driver) of
        {ok, _} ->
            case Row of
                [] ->
                    {ok, 1};
                _ ->
                    update(TableName, Selection, Row, Driver)
            end;
        {error, enotfound} ->
            case insert(TableName, rowterm(Selection) ++ Row, Driver) of
                ok ->
                    {ok, insert};
                {error, _} = E ->
                    E
            end;
        {error, _} = E ->
            E
    end.

-spec delete(persi:table(), persi:selection(), persi:connection() | #persi_driver{}) -> {ok, non_neg_integer()} | persi:error().
delete(TableName, Selection, Connection) when is_atom(TableName), is_atom(Connection) ->
    delete(TableName, Selection, persi_connection:lookup_driver(Connection));
delete(TableName, Selection, Driver = #persi_driver{module=Mod}) ->

    {Where, Args} = selection_where(Selection, Mod),
    Sql = [<<"DELETE FROM ">>, atom_to_list(TableName), " WHERE ", Where],
    case Mod:q(Sql, Args, Driver) of
        {ok, {_, _, 0}} ->
            {error, enotfound};
        {ok, {_, _, Nr}} ->
            {ok, Nr};
        {error, _} = E ->
            E
    end.

-spec select(persi:table(), persi:selection(), persi:connection() | #persi_driver{}) -> {ok, persi:row()} | persi:error().
select(TableName, Selection, Connection) when is_atom(TableName), is_atom(Connection) ->
    select(TableName, Selection, persi_connection:lookup_driver(Connection));
select(TableName, Selection, Driver = #persi_driver{module=Mod}) ->

    {Where, Args} = selection_where(Selection, Mod),
    Sql = [<<"SELECT * FROM ">>, atom_to_list(TableName), " WHERE ", Where, " LIMIT 1"],

    case Mod:q(Sql, Args, Driver) of
        {ok, {[], _, _}} ->
            {error, enotfound};
        {ok, {[Values], Columns, _}} ->
            {ok, TableInfo} = persi_schema:table_info(TableName, Driver),
            {ok, persi_util:values_to_row(Values, Columns, TableInfo)};
        {error, _} = E ->
            E
    end.


rowterm(Simple) when not(is_list(Simple)) ->
    [{id, Simple}];
rowterm(R) ->
    R.

-spec selection_where(persi:selection(), module()) -> {iolist(), persi:sql_args()}.
selection_where(X, Y) ->
    selection_where(X, Y, 1).

-spec selection_where(persi:selection(), module(), non_neg_integer()) -> {iolist(), persi:sql_args()}.
selection_where(Simple, M, N) when not(is_list(Simple)) ->
    selection_where(rowterm(Simple), M, N);
selection_where([], _, _) ->
    throw({error, empty_selection});
selection_where(KVs, Mod, StartN) when is_list(KVs) ->
    {Clauses, Args, _}
        = lists:foldl(
            fun({K, V}, {C0, A0, N}) ->
                    {[ [atom_to_list(K), " = ", ?param(N)] | C0], [V | A0], N+1}
            end,
            {[], [], StartN},
            KVs),
    {persi_util:iolist_join(lists:reverse(Clauses), " AND "), lists:reverse(Args)}.

                                   
opt_fold_props(#persi_table{has_props=false}, Row, _, _) ->
    Row;
opt_fold_props(#persi_table{has_props=true, columns=Columns, name=TableName}, Row, PK, Driver = #persi_driver{module=Mod}) ->
    ColNames = [C#persi_column.name || C <- Columns],
    %% split row
    {ColData, Props0} =
        lists:partition(fun({K, _}) -> lists:member(K, ColNames) end,
                        Row),
    Props = case PK of
                undefined ->
                    Props0;
                _ ->
                    %% merge props on update
                    {Where, WhereArgs} = selection_where(PK, Mod),
                    Sql = [<<"SELECT ">>, atom_to_list(?persi_props_column_name), <<" FROM ">>, atom_to_list(TableName), " WHERE ", Where],

                    case Mod:q(Sql, WhereArgs, Driver) of
                        {ok, {[], _, _}} ->
                            Props0;
                        {ok, {[[ExistingPropsBin]], _, _}} ->
                            merge_props(Props0, binary_to_term(ExistingPropsBin))
                    end
            end,
    [{?persi_props_column_name, term_to_binary(Props)} | ColData].

            
merge_props(New, {Old}) ->
    merge_props(New, Old); %% unwrap legacy
merge_props(New, Old) ->
    lists:foldr(
      fun({K, V}, Acc) ->
              [{K, V} | 
               case proplists:lookup(K, Acc) of
                   {K, _} -> proplists:delete(K, Acc);
                   none -> Acc
               end]
      end,
      Old,
      New).
