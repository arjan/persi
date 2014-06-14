-module(persi).

-include_lib("persi.hrl").

-export(
   [
    insert/2,
    insert/3,

    update/3,
    update/4,

    upsert/3,
    upsert/4,

    delete/2,
    delete/3,

    select/2,
    select/3
   ]).

%% Types start here
-export_type([id/0, table/0, row/0, col_name/0, col_value/0, connection/0, error/0]).

-type id() :: non_neg_integer().
-type table() :: atom().
-type row() :: [{col_name(), col_value()}].

-type col_name() :: atom().
-type col_value() :: term().

-type connection() :: atom().
-type error() :: {error, atom()}.


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
