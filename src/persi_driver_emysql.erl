%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc MySQL driver based on Eonblast's emysql application

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

-module(persi_driver_emysql).

-behaviour(gen_server).
-behaviour(persi_driver).

-include_lib("emysql/include/emysql.hrl").

-include_lib("persi/include/persi.hrl").
-include_lib("persi_int.hrl").

-record(state, {id, metadata=undefined}).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% persi_driver exports
-export(
   [
    schema_info/1,
    table_info/2,
    exec/2,
    flush_metadata/1,
    q/3,
    map_dialect/1,
    acquire_connection/1,
    release_connection/1
   ]).

%% interface functions
-export([
        ]).

%%====================================================================
%% API
%%====================================================================

schema_info(#persi_driver{pid=Pid}) ->
    gen_server:call(Pid, schema_info).

table_info(Table, #persi_driver{pid=Pid})  when is_atom(Table) ->
    gen_server:call(Pid, {table_info, Table}).

exec(Sql, #persi_driver{id=Id}) ->
    %%io:format(user, ">> ~p~n", [iolist_to_binary(Sql)]),
    #ok_packet{} = emysql:execute(Id, iolist_to_binary(Sql)),
    ok.

flush_metadata(#persi_driver{pid=Pid}) ->
    gen_server:call(Pid, flush_metadata).

q(Sql, Args, #persi_driver{id=Id}) ->
    Stmt = ?MODULE,
    try
        emysql:prepare(Stmt, iolist_to_binary(Sql)),
        case emysql:execute(Id, Stmt, Args) of
            #ok_packet{affected_rows=Rows} ->
                {ok, {[[Rows]], [], Rows}};
            #result_packet{rows=Rows, field_list=Fields} ->
                {ok, {[map_row_values(Row) || Row <- Rows], [binary_to_atom(F#field.name, utf8) || F <- Fields], 0}};
            #error_packet{msg=Msg, status=Status} ->
                {error, {emysql, Status, Msg}}
        end
    catch
        exit:{{failed_to_prepare_statement, Msg1}, _} ->
            {error, {emysql, Msg1}}
    end.

map_dialect({check_support, #persi_column{type=varchar, length=undefined}}) -> {error, varchar_without_length};
map_dialect({check_support, transactions}) -> {error, transactions_not_supported};
map_dialect({check_support, _}) -> true;
map_dialect({columntype, #persi_column{type=T}}) -> T;
map_dialect({columnvalue, _, V}) -> V;
map_dialect({sql_parameter, _N}) -> "?".

acquire_connection(#persi_driver{}) ->
    throw(transactions_not_supported).
release_connection(#persi_driver{}) ->
    throw(transactions_not_supported).
    


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
init({Id, Args}) ->
    persi_driver:reg(?MODULE),

    emysql:add_pool(Id, [{encoding,utf8},
                         {start_cmds,
                          [
                           <<"SET TIME_ZONE='+00:00'">>,
                           <<"SET SQL_MODE='STRICT_ALL_TABLES'">>
                          ]}
                         | Args]),

    {ok, #state{id=Id, metadata=do_schema_info(Id)}}.


handle_call(remove_connection, _From, State) ->
    %% Here drivers have the chance to clean up after themselves
    {reply, ok, State};

handle_call(schema_info, _From, State=#state{metadata=Metadata}) ->
    {reply, Metadata, State};

handle_call({table_info, Table}, _From, State=#state{metadata=Metadata}) ->
    Reply = case [T || T <- Metadata#persi_schema.tables, T#persi_table.name =:= Table] of
                [] ->
                    {error, enotfound};
                [Info] ->
                    {ok, Info}
            end,
    {reply, Reply, State};

handle_call(flush_metadata, _From, State) ->
    {reply, ok, State#state{metadata=do_schema_info(State#state.id)}};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc This function is called by a gen_server when it is about to terminate.
terminate(_Reason, _State) ->
    ok.

%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


do_schema_info(Connection) ->
    Tables = do_list_tables(Connection),
    #persi_schema{
       tables=[do_table_info(Table, Connection) || Table <- Tables]
      }.

do_list_tables(Id) ->
    R = emysql:execute(Id, <<"SHOW TABLES">>),
    lists:map(fun([Name]) -> erlang:binary_to_atom(Name, utf8) end, 
              R#result_packet.rows).

%% @doc Return a descripion of the table.
do_table_info(TableName, Id) ->
    Sql = iolist_to_binary([<<"DESC ">>, erlang:atom_to_binary(TableName, utf8)]),
    R = emysql:execute(Id, Sql),

    WithPK = lists:map(fun([ColumnName, ColumnType, Null, Key, Default, _Extra]) -> 
                               Column = map_columntype(ColumnType),
                               {Column#persi_column{name=erlang:binary_to_atom(ColumnName, utf8),
                                                    default=map_default(Default, ColumnType),
                                                    notnull=Null =:= 0}, Key =:= <<"PRI">>}
                       end,
                       R#result_packet.rows),

    {Cols, {PKs, HasProps}} =
        lists:mapfoldl(fun({C=#persi_column{name=Name}, true}, {Acc, HasProps}) ->
                               {C, {[Name|Acc], HasProps orelse Name =:= ?persi_props_column_name}};
                          ({C=#persi_column{name=Name}, false}, {Acc, HasProps}) ->
                               {C, {Acc, HasProps orelse Name =:= ?persi_props_column_name}} end,
                       {[], false},
                       WithPK),

    Sql1 = [<<"SELECT column_name, referenced_table_name, referenced_column_name FROM information_schema.key_column_usage WHERE table_name = '">>, erlang:atom_to_binary(TableName, utf8), <<"' AND referenced_table_name IS NOT NULL">>],
    R1 = emysql:execute(Id, iolist_to_binary(Sql1)),

    FKs = lists:map(fun([From, Table, To]) ->
                            #persi_fk{table=erlang:binary_to_atom(Table, utf8),
                                      from=erlang:binary_to_atom(From, utf8),
                                      to=erlang:binary_to_atom(To, utf8)}
                    end,
                    R1#result_packet.rows),

    #persi_table{
       name=TableName,
       columns=Cols,
       pk=PKs,
       fks=FKs,
       has_props=HasProps}.


-spec map_columntype(binary()) -> #persi_column{}.
map_columntype(<<"int(", _/binary>>) ->
    #persi_column{type=int};
map_columntype(<<"tinyint(1)">>) ->
    #persi_column{type=boolean};
map_columntype(X) ->
    case persi_util:map_sql_to_column(X) of
        undefined ->
            #persi_column{type=binary_to_atom(X, utf8)};
        C ->
            C
    end.

map_default(Value, <<"int(", _/binary>>) when is_binary(Value) -> 
    list_to_integer(binary_to_list(Value));
map_default(<<"1">>, <<"tinyint(1)">>) -> true;
map_default(<<"0">>, <<"tinyint(1)">>) -> false;
map_default(V, _) -> V.


map_row_values(Row) ->
    [map_value(V) || V <- Row].

map_value({datetime, DT}) ->
    DT;
map_value(V) ->
    V.
