%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Test table related functions

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

-module(persi_table_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("persi/include/persi.hrl").

-define(DBFILE, ":memory:").

setup() ->
    application:start(gproc),
    application:start(persi),

    ok = persi:add_connection([{driver, persi_driver_esqlite}, {dbfile, ?DBFILE}]),

    Table = #persi_table{name=demotable,
                         columns=
                             [
                              #persi_column{name=id, type=int},
                              #persi_column{name=name, type="varchar(50)", default="app", notnull=true}
                             ],
                        pk=[id]},
    persi:create_table(Table),
    
    ok.


full_test() ->
    setup(),
    
    {error, enotfound} = persi:select(demotable, 123),
    {error, enotfound} = persi:delete(demotable, 123),

    ok = persi:insert(demotable, [{id, 123}, {name, <<"Foo">>}]),

    {ok, Row} = persi:select(demotable, 123),
    123 = proplists:get_value(id, Row),
    <<"Foo">> = proplists:get_value(name, Row),

    {ok, 1} = persi:update(demotable, 123, [{name, <<"Bar">>}]),
    {ok, Row1} = persi:select(demotable, 123),
    123 = proplists:get_value(id, Row1),
    <<"Bar">> = proplists:get_value(name, Row1),
    
    {ok, 1} = persi:delete(demotable, 123),
    {error, enotfound} = persi:select(demotable, 123),

    persi:remove_connection(),
    
    ok.



upsert_test() ->
    setup(),
    
    {ok, insert} = persi:upsert(demotable, 111, [{name, <<"Foo">>}]),

    {ok, _} = persi:select(demotable, 111),

    {ok, 1} = persi:upsert(demotable, 111, [{name, <<"Bar">>}]),
    {ok, Row1} = persi:select(demotable, 111),
    111 = proplists:get_value(id, Row1),
    <<"Bar">> = proplists:get_value(name, Row1),
    
    {ok, 1} = persi:delete(demotable, 111),

    persi:remove_connection(),
    
    ok.


unknown_column_test() ->
    setup(),
    
    {error, enotfound} = persi:select(demotable, 123),

    {error, {sqlite_error, _}} = persi:insert(demotable, [{id, 123}, {name, <<"Foo">>}, {meh, moeder}]),
    {error, {sqlite_error, _}} = persi:update(demotable, 123, [{name, <<"Foo">>}, {meh, moeder}]),
    {error, {sqlite_error, _}} = persi:upsert(demotable, 123, [{name, <<"Foo">>}, {meh, moeder}]),
    
    persi:remove_connection(),
    
    ok.