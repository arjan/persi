%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Pgsql worker (based on Poolboy README)

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


-module(persi_pgsql_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {conn, log_queries=false}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    Hostname = proplists:get_value(hostname, Args, "localhost"),
    Database = proplists:get_value(database, Args, ""),
    Username = proplists:get_value(user, Args, "postgres"),
    Password = proplists:get_value(password, Args, ""),
    {ok, Conn} = pgsql:connect(Hostname, Username, Password, [
        {database, Database}
    ]),

    InitialQueries = proplists:get_value(initial_queries, Args, []),
    [pgsql:squery(Conn, Query) || Query <- InitialQueries],

    LogQueries = case application:get_env(persi, log_queries) of
                     {ok, Value} -> Value;
                     undefined -> false
                 end,

    {ok, #state{conn=Conn, log_queries=LogQueries}}.

handle_call({squery, Sql}, _From, #state{conn=Conn}=State) ->
    opt_log_queries(Sql, [], State),
    {reply, pgsql:squery(Conn, Sql), State};
handle_call({equery, Stmt, Params}, _From, #state{conn=Conn}=State) ->
    opt_log_queries(Stmt, Params, State),
    {reply, pgsql:equery(Conn, Stmt, Params), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
    ok = pgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

opt_log_queries(Sql, Args, #state{log_queries={M, F}}) ->
    M:F("[pgsql ~p] Query: ~s, ~p", [self(), Sql, Args]);
opt_log_queries(Sql, Args, #state{log_queries=F}) when is_function(F) ->
    F("[pgsql ~p] Query: ~s, ~p~n", [self(), Sql, Args]);
opt_log_queries(_, _, _) ->
    ok.
