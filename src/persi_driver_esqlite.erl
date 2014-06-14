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

-behaviour(persi_driver).
-behaviour(gen_server).

-include_lib("persi/include/persi.hrl").
   
-record(state, {db}).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% presi_driver exports
-export([schema_info/1, table_info/2, exec/2]).

%% interface functions
-export([
        ]).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).


schema_info(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, schema_info).


table_info(Table, Pid)  when is_atom(Table), is_pid(Pid) ->
    gen_server:call(Pid, {table_info, Table}).

exec(Sql, Pid) ->
    gen_server:call(Pid, {exec, Sql}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
init(Args) ->
    persi_driver:reg(?MODULE),

    {dbfile, DbFile} = proplists:lookup(dbfile, Args),

    %% Open the file
    {ok, Db} = esqlite3:open(DbFile),
    
    %% Enable foreign key checks
    ok = esqlite3:exec(<<"PRAGMA foreign_keys=1;">>, Db),
    
    %% Assert a lock on the db file, no 2 processes can open the same db file at once
    %%%Fixme? gproc:reg_shared({p,l,{esqlite_dbfile, DbFile}}),

    {ok, #state{db=Db}}.


handle_call(schema_info, _From, State) ->
    {reply, do_schema_info(State#state.db), State};

handle_call({table_info, Table}, _From, State) ->
    {reply, do_table_info(Table, State#state.db), State};

handle_call({exec, Sql}, _From, State) ->
    {reply, esqlite3:exec(iolist_to_binary(Sql), State#state.db), State};

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
                                  {#persi_column{name=erlang:binary_to_atom(ColumnName, utf8),
                                                 type=ColumnType,
                                                 default=Default,
                                                 notnull=NotNull =/= 0}, PrimaryKey =/= 0}
                          end,
                          [<<"PRAGMA table_info('">>, erlang:atom_to_binary(TableName, utf8), <<"');">>], Connection),
    {Cols, PKs} = lists:mapfoldl(fun({C=#persi_column{name=Name}, true}, Acc) -> {C, [Name|Acc]};
                                    (C, Acc) -> {C, Acc} end,
                                 [],
                                 WithPK),

    FKs = esqlite3:map(fun({_Cid, _Seq, Table, From, To, _OnUpdate, _OnDelete, _Match}) -> 
                               #persi_fk{table=erlang:binary_to_atom(Table, utf8),
                                         from=erlang:binary_to_atom(From, utf8),
                                         to=erlang:binary_to_atom(To, utf8)}
                       end,
                          [<<"PRAGMA foreign_key_list('">>, erlang:atom_to_binary(TableName, utf8), <<"');">>], Connection),
    case Cols of
        [] ->
            {error, enotfound};
        _ -> 
            #persi_table{
               name=TableName,
               columns=Cols,
               pk=PKs,
               fks=FKs}
    end.

