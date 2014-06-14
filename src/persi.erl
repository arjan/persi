%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Main access module for the persi application

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

-module(persi).

-define(DEFAULT_CONNECTION, default).

-include_lib("persi.hrl").

-export(
   [
    add_connection/2,
    
    insert/2,
    insert/3,

    update/3,
    update/4,

    upsert/3,
    upsert/4,

    delete/2,
    delete/3,

    select/2,
    select/3,

    schema_info/0,
    schema_info/1,

    table_info/1,
    table_info/2
    
   ]).

%% Types start here
-export_type([id/0, table/0, row/0, col_name/0, col_value/0, connection/0, error/0]).

-type error() :: {error, atom()}.

-type id() :: non_neg_integer().
-type table() :: atom().
-type row() :: [{col_name(), col_value()}].

-type col_name() :: atom().
-type col_value() :: term().

-type connection() :: atom().
-type connection_opts() :: [connection_opt()].
-type connection_opt() :: {dbdriver, module()}.


%%% CONNECTION %%%

-spec add_connection(connection(), connection_opts()) -> ok | {error, eexist}.
add_connection(Conn, Opts) ->
    persi_connection:add(Conn, Opts).


%%% TABLE %%%

-spec insert(table(), row()) -> {ok, id()}.
insert(Table, Data) ->
    insert(Table, Data, ?DEFAULT_CONNECTION).

-spec insert(table(), row(), connection()) -> {ok, id()}.
insert(Table, Data, Conn) ->
    persi_table:insert(Table, Data, Conn).


-spec update(table(), id(), row()) -> {ok, id()}.
update(Table, Id, Data) ->
    update(Table, Id, Data, ?DEFAULT_CONNECTION).

-spec update(table(), id(), row(), connection()) -> {ok, id()}.
update(Table, Id, Data, Conn) ->
    persi_table:update(Table, Id, Data, Conn).


-spec upsert(table(), id(), row()) -> {ok, id()}.
upsert(Table, Id, Data) ->
    upsert(Table, Id, Data, ?DEFAULT_CONNECTION).

-spec upsert(table(), id(), row(), connection()) -> {ok, id()}.
upsert(Table, Id, Data, Conn) ->
    persi_table:upsert(Table, Id, Data, Conn).


-spec delete(table(), id()) -> ok | error().
delete(Table, Id) ->
    delete(Table, Id, ?DEFAULT_CONNECTION).

-spec delete(table(), id(), connection()) -> {ok, id()}.
delete(Table, Id, Conn) ->
    persi_table:delete(Table, Id, Conn).


-spec select(table(), id()) -> {ok, row()} | error().
select(Table, Id) ->
    select(Table, Id, ?DEFAULT_CONNECTION).

-spec select(table(), id(), connection()) -> {ok, row()} | error().
select(Table, Id, Conn) ->
    persi_table:select(Table, Id, Conn).


%%% SCHEMA %%%

-spec schema_info() -> persi_schema:info().
schema_info() ->
    schema_info(?DEFAULT_CONNECTION).

-spec schema_info(connection()) -> persi_schema:info().
schema_info(Conn) ->
    persi_schema:info(Conn).


-spec table_info(table()) -> persi_schema:table_info().
table_info(Table) ->
    table_info(Table, ?DEFAULT_CONNECTION).

-spec table_info(table(), connection()) -> persi_schema:table_info().
table_info(Table, Conn) ->
    persi_schema:table_info(Table, Conn).

