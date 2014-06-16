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

-module(persi_table_anydata_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("persi/include/persi.hrl").

-include("persi_eunit.hrl").

-define(complicated_term, [<<"foo fdfd fds fads fdsa fdsa fdsa fdsa fsdfdfdf">>, [{bar, baz, 12332, 232.222}]]).


demotable() ->
    Table = #persi_table{name=demotable,
                         columns=
                             [
                              #persi_column{name=id, type=int},
                              ?persi_props_column
                             ],
                        pk=[id]},
    persi:create_table(Table),
    
    ok.


full_test() ->
    setup(),
    demotable(),
    
    {error, enotfound} = persi:select(demotable, 123),
    {error, enotfound} = persi:delete(demotable, 123),

    {ok, T1} = persi:table_info(demotable),
    true = T1#persi_table.has_props,
    
    ok = persi:insert(demotable, [{id, 123}, {name, <<"Foo">>}, {data, ?complicated_term}]),

    {ok, Row} = persi:select(demotable, 123),
    123 = proplists:get_value(id, Row),
    <<"Foo">> = proplists:get_value(name, Row),
    ?complicated_term = proplists:get_value(data, Row),
    
    {ok, 1} = persi:update(demotable, 123, [{name, <<"Bar">>}]),
    {ok, Row1} = persi:select(demotable, 123),
    123 = proplists:get_value(id, Row1),
    <<"Bar">> = proplists:get_value(name, Row1),
    ?complicated_term = proplists:get_value(data, Row1),
    
    {ok, 1} = persi:delete(demotable, 123),
    {error, enotfound} = persi:select(demotable, 123),

    teardown().


upsert_test() ->
    setup(),
    demotable(),
    
    {ok, insert} = persi:upsert(demotable, 111, [{name, <<"Foo">>}]),
    {ok, insert} = persi:upsert(demotable, [{id, 33333}], [{name, <<"Another row">>}]),
    
    {ok, _} = persi:select(demotable, 111),

    {ok, 1} = persi:upsert(demotable, 111, [{name, <<"Bar">>}]),
    {ok, Row1} = persi:select(demotable, 111),

    %% alternate selection mechanisms
    {ok, Row1} = persi:select(demotable, [{id, 111}]),
    
    111 = proplists:get_value(id, Row1),
    <<"Bar">> = proplists:get_value(name, Row1),
    
    {ok, 1} = persi:delete(demotable, 111),

    teardown().


%% A legacy system used to have the 'props' column encoded as a
%% jiffy-compatible proplist, e.g. a {proplist()}. These tests make
%% sure that data format is still supported.
legacy_test() ->
    setup(),
    demotable(),
    
    %% insert some legacy data
    InsertQ = case os:getenv("PERSI_DBDRIVER") of
                  "pgsql" -> "INSERT INTO demotable (id, props) VALUES ($1, $2)";
                  _ -> "INSERT INTO demotable (id, props) VALUES (?, ?)"
              end,
    persi_query:fetchall(InsertQ, [444, term_to_binary( {[{foo, bar}]} )]),

    {ok, Row1} = persi:select(demotable, [{id, 444}]),
    
    444 = proplists:get_value(id, Row1),
    bar = proplists:get_value(foo, Row1),

    %% update
    {ok, 1} = persi:update(demotable, 444, [{name, <<"Bar">>}]),
    {ok, Row2} = persi:select(demotable, 444),
    <<"Bar">> = proplists:get_value(name, Row2),
    bar = proplists:get_value(foo, Row2),

    teardown().

column_migrate_test() ->
    setup(),
    demotable(),
    
    {ok, T1} = persi:table_info(demotable),
    true = T1#persi_table.has_props,

    %% here, 'name' is stored in the props column
    ok = persi:insert(demotable, [{id, 123}, {name, <<"Foo">>}]),

    %% add_column will automatically move the 'name' property into its own column
    persi:add_column(demotable, #persi_column{name=name, type="varchar(255)"}),

    %% check that the data is now in the column
    {ok, {[[123, <<"Foo">>]], _, _}} = persi_query:fetchall(<<"SELECT id, name FROM demotable">>, []),

    %% check that the props are now empty
    {ok, {[[Empty]], _, _}} = persi_query:fetchall(<<"SELECT props FROM demotable">>, []),
    [] = binary_to_term(Empty),

    teardown().
