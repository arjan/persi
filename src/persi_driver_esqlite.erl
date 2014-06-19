%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc ESqlite driver (esqlite3 database)

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

-module(persi_driver_esqlite).

-behaviour(gen_server).
-behaviour(persi_driver).

-include_lib("persi/include/persi.hrl").
-include("persi_int.hrl").

-record(state, {id, db, metadata=undefined}).

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

exec(Sql, #persi_driver{pid=Pid}) ->
    gen_server:call(Pid, {exec, iolist_to_binary(Sql)}).

flush_metadata(#persi_driver{pid=Pid}) ->
    gen_server:call(Pid, flush_metadata).

q(Sql, Args, #persi_driver{pid=Pid}) ->
    gen_server:call(Pid, {q, iolist_to_binary(Sql), Args}).

map_dialect({check_support, drop_column}) -> {error, drop_column_not_supported};
map_dialect({check_support, _}) -> true;
map_dialect({columntype, #persi_column{type=T}}) -> T;
map_dialect({sql_parameter, _N}) -> "?".


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
init({Id, Args}) ->
    persi_driver:reg(?MODULE),

    {dbfile, DbFile} = proplists:lookup(dbfile, Args),

    %% Open the file
    {ok, Db} = esqlite3:open(DbFile),

    %% Enable foreign key checks
    ok = esqlite3:exec(<<"PRAGMA foreign_keys=1;">>, Db),
    ok = esqlite3:exec(<<"PRAGMA count_changes=1;">>, Db),

    %% Assert a lock on the db file, no 2 processes can open the same db file at once
%%%Fixme? gproc:reg_shared({p,l,{esqlite_dbfile, DbFile}}),

    {ok, #state{id=Id, db=Db, metadata=do_schema_info(Db)}}.


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

handle_call({exec, Sql}, _From, State) ->
    {reply, esqlite3:exec(iolist_to_binary(Sql), State#state.db), State};

handle_call({q, Sql, Args}, _From, State) ->
    Result = case esqlite3:prepare(iolist_to_binary(Sql), State#state.db) of
                 {ok, Stmt} ->
                     ColNames = esqlite3:column_names(Stmt),
                     ok = esqlite3:bind(Stmt, Args),
                     case esqlite3:fetchall(Stmt) of
                         Ret when is_list(Ret) ->
                             {ok, NumRows} = esqlite3:changes(State#state.db),
                             {ok, {[tuple_to_list(R) || R <- Ret], tuple_to_list(ColNames), NumRows}};
                         {error, _} = E ->
                             E
                     end;
                 {error, _} = E ->
                     E
             end,
    {reply, Result, State};

handle_call(flush_metadata, _From, State) ->
    {reply, ok, State#state{metadata=do_schema_info(State#state.db)}};

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
terminate(_Reason, State) ->
    esqlite3:close(State#state.db),
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

do_list_tables(Connection) ->
    esqlite3:map(fun({Name}) -> erlang:binary_to_atom(Name, utf8) end, 
                 <<"SELECT name FROM sqlite_master WHERE type='table' ORDER by name;">>, Connection).

%% @doc Return a descripion of the table.
do_table_info(TableName, Connection) ->
    WithPK = esqlite3:map(fun({_Cid, ColumnName, ColumnType, NotNull, Default, PrimaryKey}) ->
                                  Column = map_columntype(ColumnType),
                                  {Column#persi_column{name=erlang:binary_to_atom(ColumnName, utf8),
                                                       default=map_value(ColumnType, Default),
                                                       notnull=NotNull =/= 0}, PrimaryKey =/= 0}
                          end,
                          [<<"PRAGMA table_info('">>, erlang:atom_to_binary(TableName, utf8), <<"');">>], Connection),
    {Cols, {PKs, HasProps}} =
        lists:mapfoldl(fun({C=#persi_column{name=Name}, true}, {Acc, HasProps}) ->
                               {C, {[Name|Acc], HasProps orelse Name =:= ?persi_props_column_name}};
                          ({C=#persi_column{name=Name}, false}, {Acc, HasProps}) ->
                               {C, {Acc, HasProps orelse Name =:= ?persi_props_column_name}} end,
                       {[], false},
                       WithPK),
    FKs = esqlite3:map(fun({_Cid, _Seq, Table, From, To, _OnUpdate, _OnDelete, _Match}) -> 
                               #persi_fk{table=erlang:binary_to_atom(Table, utf8),
                                         from=erlang:binary_to_atom(From, utf8),
                                         to=erlang:binary_to_atom(To, utf8)}
                       end,
                       [<<"PRAGMA foreign_key_list('">>, erlang:atom_to_binary(TableName, utf8), <<"');">>], Connection),
    #persi_table{
       name=TableName,
       columns=Cols,
       pk=PKs,
       fks=FKs,
       has_props=HasProps}.


-spec map_columntype(binary()) -> #persi_column{}.
map_columntype(X) ->
    case persi_util:map_sql_to_column(X) of
        undefined ->
            #persi_column{type=binary_to_atom(X, utf8)};
        C ->
            C
    end.


map_value(_, <<"false">>) -> false;
map_value(_, <<"true">>) -> true;
map_value(<<"int">>, V) when is_binary(V) ->
    list_to_integer(binary_to_list(map_value(unknown, V)));
map_value(_, <<"'", Rest/binary>>) ->
    hd(binary:split(Rest, <<"'">>));
map_value(_T, X) ->
    X.
