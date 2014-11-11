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

-include_lib("persi/include/persi.hrl").

-export(
   [
    %% persi_connection wrappers
    add_connection/2,
    add_connection/3,
    remove_connection/0,
    remove_connection/1,

    %% persi_table wrappers
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

    %% persi_schema wrappers
    schema_info/0,
    schema_info/1,
    table_info/1,
    table_info/2,
    create_table/1,
    create_table/2,
    drop_table/1,
    drop_table/2,
    add_column/2,
    add_column/3,
    drop_column/2,
    drop_column/3,
    manage_schema/1,
    manage_schema/2,
    flush_metadata/0,
    flush_metadata/1,
    
    %% persi_query wrappers
    q/1,
    q/2,
    q/3,
    rows/3,
    rows/4,
    transaction/1,
    transaction/2
    
   ]).

%% Types start here
-export_type(
   [
    id/0,
    table/0,
    schema_info/0,
    table_info/0,
    column_info/0,
    selection/0,
    row/0,
    col_name/0,
    col_value/0,
    connection/0,
    connection_opts/0,
    error/0,
    sql/0,
    sql_args/0,
    q_result/0,
    manage_result/0,
    column_names/0,
    column_type/0
   ]).

-type error() :: {error, atom()}.

-type selection() :: binary() | integer() | atom() | [{col_name(), col_value()}].
-type id() :: non_neg_integer() | binary() | atom().

-type row() :: [{col_name(), col_value()}].
-type col_name() :: atom().
-type col_value() :: term().

-type table() :: atom().
-type connection() :: atom().
-type connection_opts() :: [connection_opt()].
-type connection_opt() :: {driver, module()} | {atom(), term()}.

-type schema_info() :: #persi_schema{}.
-type table_info() :: #persi_table{}.
-type column_info() :: #persi_column{}.
-type manage_result() :: install | noop | {upgrade, non_neg_integer()}.

-type sql() :: binary() | iolist().
-type sql_args() :: [] | [term()].
-type sql_row() :: list(term()).

-type column_names() :: list(atom()).
-type column_type() :: varchar | int | decimal | bool | datetime | blob.

-type q_result() :: {ok, {[sql_row()], column_names(), non_neg_integer()}} | persi:error().        


%%% CONNECTION %%%
-spec add_connection(module(), connection_opts()) -> ok | {error, eexist}.
add_connection(DriverModule, Opts) ->
    add_connection(?PERSI_DEFAULT_CONNECTION, DriverModule, Opts).

-spec add_connection(connection(), module(), connection_opts()) -> ok | {error, eexist}.
add_connection(Conn, DriverModule, Opts) ->
    persi_connection:add(Conn, DriverModule, Opts).

-spec remove_connection() -> ok | {error, enotfound}.
remove_connection() ->
    remove_connection(?PERSI_DEFAULT_CONNECTION).

-spec remove_connection(connection()) -> ok | {error, enotfound}.
remove_connection(Conn) ->
    persi_connection:remove(Conn).


%%% TABLE %%%

%% @doc Insert a single row into the given table using the default connection.
-spec insert(table(), row()) -> ok | error().
insert(Table, Data) ->
    insert(Table, Data, ?PERSI_DEFAULT_CONNECTION).

%% @doc Insert a single row into the given table.
-spec insert(table(), row(), connection()) -> ok | error().
insert(Table, Data, Conn) ->
    persi_table:insert(Table, Data, Conn).


-spec update(table(), id(), row()) -> {ok, non_neg_integer()} | error().
update(Table, Id, Data) ->
    update(Table, Id, Data, ?PERSI_DEFAULT_CONNECTION).

%% @doc Update a row or a selection of rows in the given table.
-spec update(table(), id(), row(), connection()) -> {ok, non_neg_integer()} | error().
update(Table, Id, Data, Conn) ->
    persi_table:update(Table, Id, Data, Conn).


-spec upsert(table(), id(), row()) -> {ok, non_neg_integer()} | error().
upsert(Table, Id, Data) ->
    upsert(Table, Id, Data, ?PERSI_DEFAULT_CONNECTION).

-spec upsert(table(), id(), row(), connection()) -> {ok, non_neg_integer()} | error().
upsert(Table, Id, Data, Conn) ->
    persi_table:upsert(Table, Id, Data, Conn).


-spec delete(table(), id()) -> {ok, non_neg_integer()} | error().
delete(Table, Id) ->
    delete(Table, Id, ?PERSI_DEFAULT_CONNECTION).

-spec delete(table(), id(), connection()) -> {ok, non_neg_integer()} | error().
delete(Table, Id, Conn) ->
    persi_table:delete(Table, Id, Conn).


-spec select(table(), id()) -> {ok, row()} | error().
select(Table, Id) ->
    select(Table, Id, ?PERSI_DEFAULT_CONNECTION).

-spec select(table(), id(), connection()) -> {ok, row()} | error().
select(Table, Id, Conn) ->
    persi_table:select(Table, Id, Conn).


%%% SCHEMA %%%

-spec schema_info() -> #persi_schema{}.
schema_info() ->
    schema_info(?PERSI_DEFAULT_CONNECTION).

-spec schema_info(connection()) -> schema_info().
schema_info(Conn) ->
    persi_schema:info(Conn).


-spec table_info(table()) -> {ok, table_info()} | {error, enotfound}.
table_info(Table) ->
    table_info(Table, ?PERSI_DEFAULT_CONNECTION).

-spec table_info(table(), connection()) -> {ok, table_info()} | {error, enotfound}.
table_info(Table, Conn) ->
    persi_schema:table_info(Table, Conn).


-spec create_table(table_info()) -> ok | {error, eexist}.
create_table(Table) ->
    create_table(Table, ?PERSI_DEFAULT_CONNECTION).

-spec create_table(table_info(), connection()) -> ok | {error, eexist}.
create_table(Table, Conn) ->
    persi_schema:create_table(Table, Conn).

-spec drop_table(table()) -> ok | {error, enotfound}.
drop_table(Table) ->
    drop_table(Table, ?PERSI_DEFAULT_CONNECTION).

-spec drop_table(table(), connection()) -> ok | {error, enotfound}.
drop_table(Table, Conn) ->
    persi_schema:drop_table(Table, Conn).

-spec add_column(table(), column_info()) -> ok | {error, enotfound}.
add_column(Table, Column) ->
    add_column(Table, Column, ?PERSI_DEFAULT_CONNECTION).

-spec add_column(table(), #persi_column{}, connection()) -> ok | {error, enotfound}.
add_column(Table, Column, Conn) ->
    persi_schema:add_column(Table, Column, Conn).

-spec drop_column(table(), atom()) -> ok | {error, enotfound}.
drop_column(Table, ColumnName) ->
    drop_column(Table, ColumnName, ?PERSI_DEFAULT_CONNECTION).

-spec drop_column(table(), atom(), connection()) -> ok | {error, enotfound}.
drop_column(Table, ColumnName, Conn) ->
    persi_schema:drop_column(Table, ColumnName, Conn).


-spec manage_schema(module()) -> manage_result().
manage_schema(Module) ->
    manage_schema(Module, ?PERSI_DEFAULT_CONNECTION).

-spec manage_schema(module(), connection()) -> manage_result().
manage_schema(Module, Conn) ->
    persi_schema:manage(Module, Conn).


-spec flush_metadata() -> ok.
flush_metadata() ->
    flush_metadata(?PERSI_DEFAULT_CONNECTION).

-spec flush_metadata(connection()) -> ok.
flush_metadata(Conn) ->
    persi_schema:flush_metadata(Conn).


%% persi_query wrappers

-spec q(sql()) -> q_result().
q(Sql) ->
    q(Sql, [], ?PERSI_DEFAULT_CONNECTION).
-spec q(sql(), sql_args()) -> q_result().
q(Sql, Args) ->
    q(Sql, Args, ?PERSI_DEFAULT_CONNECTION).

-spec q(sql(), sql_args(), connection()) -> q_result().
q(Sql, Args, Connection) ->
    persi_query:q(Sql, Args, Connection).



-spec rows(table(), sql(), sql_args()) -> q_result().
rows(Table, Sql, Args) ->
    rows(Table, Sql, Args, ?PERSI_DEFAULT_CONNECTION).

-spec rows(table(), sql(), sql_args(), connection()) -> q_result().
rows(Table, Sql, Args, Connection) ->
    persi_query:rows(Table, Sql, Args, Connection).


-spec transaction(fun()) -> term().
transaction(F) when is_function(F) ->
    transaction(F, ?PERSI_DEFAULT_CONNECTION).

-spec transaction(fun(), connection()) -> term().
transaction(F, Conn) when is_function(F) ->
    persi_query:transaction(F, Conn).
