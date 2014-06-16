%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc PostgreSQL driver based on epgsql application

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

-module(persi_driver_epgsql).

-behaviour(gen_server).
-behaviour(persi_driver).

-include_lib("persi/include/persi.hrl").
-include_lib("persi_int.hrl").

-record(state, {id, pool, args, metadata=undefined}).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% persi_driver exports
-export(
   [
    schema_info/1,
    table_info/2,
    exec/2,
    flush_metadata/1,
    fetchall/3,
    map_dialect/1
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

exec(Sql, Driver=#persi_driver{}) ->
    squery(locate_pool(Driver), iolist_to_binary(Sql)).
    
flush_metadata(#persi_driver{pid=Pid}) ->
    gen_server:call(Pid, flush_metadata).

fetchall(Sql, Args, Driver=#persi_driver{}) ->
    case equery(locate_pool(Driver), iolist_to_binary(Sql), Args) of
        {ok, Cols, Rows} ->
            {ok, {[tuple_to_list(R) || R <- Rows],
                  [binary_to_atom(element(2, C), utf8) || C <- Cols],
                  0}};
        {ok, Count} when is_integer(Count) ->
            {ok, {[[Count]], [], Count}};
        {error, _} = E ->
            E
    end.

map_dialect({columntype, blob}) -> <<"bytea">>;
map_dialect({columntype, X}) -> X;
map_dialect({sql_parameter, N}) -> [$$ , $0 + N].  %% $1, $2, etc


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
init({Id, Args}) ->
    persi_driver:reg(?MODULE),

    PoolSize = proplists:get_value(pool_size, Args, 10),
    
    {ok, Pool} = poolboy:start_link(
                   [{worker_module, persi_pgsql_worker},
                    {size, PoolSize},
                    {max_overflow, 0}],
                   Args),
    gproc:reg({p, l, persi_pgsql_pool}, Pool),
    
    {ok, #state{id=Id, pool=Pool, args=Args, metadata=do_schema_info(Args, Pool)}}.

handle_call(remove_connection, _From, State=#state{pool=Pool}) ->
    poolboy:stop(Pool),
    {reply, ok, State#state{pool=undefined}};
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
    {reply, ok, State#state{metadata=do_schema_info(State#state.args, State#state.pool)}};

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


do_schema_info(Args, Pool) ->
    Tables = do_list_tables(Args, Pool),
    #persi_schema{
       tables=[do_table_info(Table, Args, Pool) || Table <- Tables]
      }.

do_list_tables(Args, Pool) ->
    {database, Db} = proplists:lookup(database, Args),
    DbSchema = proplists:get_value(schema, Args, "public"),
    {ok, _, Tables}
        = equery(Pool,
               "select table_name 
                from information_schema.tables 
                where table_catalog = $1 
                  and table_schema = $2
                  and table_type = 'BASE TABLE'", [Db, DbSchema]),
    [binary_to_atom(T, utf8) || {T} <- Tables].

%% equery(
    %% poolboy:transaction(
    %%   Pool, fun(Worker) ->
    %%                 persi_pgsql_worker:equery(
                                
    %% R = emysql:execute(Id, <<"SHOW TABLES">>),
    %% lists:map(fun([Name]) -> erlang:binary_to_atom(Name, utf8) end, 
    %%           R#result_packet.rows).

%% @doc Return a descripion of the table.
do_table_info(TableName, Args, Pool) ->
    {database, Db} = proplists:lookup(database, Args),
    DbSchema = proplists:get_value(schema, Args, "public"),

    {ok, _, RawCols} =
        equery(Pool,
               "  select column_name, data_type, is_nullable, column_default
                        from information_schema.columns
                        where table_catalog = $1
                          and table_schema = $2
                          and table_name = $3
                        order by ordinal_position", [Db, DbSchema, TableName]),
    
    {Cols, HasProps} = lists:foldr(
                         fun({ColumnName, ColumnType, Null, Default}, {Cols0, H}) -> 
                                 {[#persi_column{name=erlang:binary_to_atom(ColumnName, utf8),
                                                 type=ColumnType,
                                                 default=Default,
                                                 notnull= Null =:= <<"NO">>} | Cols0],
                                  H orelse binary_to_atom(ColumnName, utf8) =:= ?persi_props_column_name}
                         end,
                         {[], false},
                         RawCols),

    {ok, _, RawFKs} = 
        equery(Pool,
               "SELECT
                kcu.column_name, ccu.table_name AS foreign_table_name,
                    ccu.column_name AS foreign_column_name 
                FROM 
                    information_schema.table_constraints AS tc 
                    JOIN information_schema.key_column_usage AS kcu
                      ON tc.constraint_name = kcu.constraint_name
                    JOIN information_schema.constraint_column_usage AS ccu
                      ON ccu.constraint_name = tc.constraint_name
                WHERE constraint_type = 'FOREIGN KEY' AND tc.table_name=$1", [TableName]),
    FKs = lists:map(fun({From, Table, To}) ->
                            #persi_fk{table=erlang:binary_to_atom(Table, utf8),
                                      from=erlang:binary_to_atom(From, utf8),
                                      to=erlang:binary_to_atom(To, utf8)}
                    end,
                    RawFKs),

    {ok, _, RawPKs} = 
        equery(Pool,
               "SELECT column_name FROM 
                    information_schema.table_constraints AS tc 
                    JOIN information_schema.key_column_usage AS kcu
                      ON tc.constraint_name = kcu.constraint_name
                WHERE constraint_type = 'PRIMARY KEY' AND tc.table_name=$1", [TableName]),
    PKs = lists:map(fun({C}) -> binary_to_atom(C, utf8) end, RawPKs),
    
    #persi_table{
       name=TableName,
       columns=Cols,
       pk=PKs,
       fks=FKs,
       has_props=HasProps}.


squery(Pool, Sql) ->
    poolboy:transaction(
      Pool,
      fun(Worker) ->
              case gen_server:call(Worker, {squery, Sql}) of
                  {ok, _, _} -> ok;
                  {error, _} = E -> E
              end
      end).

equery(Pool, Stmt, Params) ->
    poolboy:transaction(Pool, fun(Worker) ->
        gen_server:call(Worker, {equery, Stmt, Params})
    end).

locate_pool(#persi_driver{pid=Pid}) ->
    gproc:get_value({p, l, persi_pgsql_pool}, Pid).
