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

-include("persi_eunit.hrl").

demotable() ->
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
    demotable(),
    
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

    {error, nodata} = persi:update(demotable, 123, []),
    
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
    {ok, Row1} = persi:select(demotable, [{name, <<"Bar">>}]),
    
    111 = proplists:get_value(id, Row1),
    <<"Bar">> = proplists:get_value(name, Row1),
    
    {ok, 1} = persi:delete(demotable, 111),

    {ok, insert} = persi:upsert(demotable, [{id, 123}], []),
    {ok, 1} = persi:upsert(demotable, [{id, 123}], []),
    {ok, 1} = persi:upsert(demotable, [{id, 123}], []),
    
    teardown().

upsert2_test() ->
    setup(),

    persi:create_table(
      #persi_table{
         name=user_seen,
         columns=
             [
              #persi_column{name=user_id, type=varchar, length=255, notnull=true},
              #persi_column{name=article_id, type=varchar, length=255, notnull=true}
             ],
         pk=[user_id,article_id]}),
    
    {ok, insert} = persi:upsert(user_seen, [{user_id, "a"}, {article_id, "b"}], []),
    {ok, 1} = persi:upsert(user_seen, [{user_id, "a"}, {article_id, "b"}], []),

    
    teardown().

unknown_table_test() ->
    setup(),
    
    {error, _} = persi:delete(unknowntable, 123),
    
    teardown().

empty_selection_test() ->
    setup(),
    demotable(),
    
    case catch persi:select(demotable, []) of
        {error, empty_selection} -> ok
    end,

    teardown().

unknown_column_test() ->
    setup(),
    demotable(),
    
    {error, enotfound} = persi:select(demotable, 123),

    {error, _} = persi:select(demotable, [{xxxid, 123}]),
    {error, _} = persi:insert(demotable, [{id, 123}, {name, <<"Foo">>}, {meh, moeder}]),
    {error, _} = persi:update(demotable, 123, [{name, <<"Foo">>}, {meh, moeder}]),
    {error, _} = persi:upsert(demotable, 123, [{name, <<"Foo">>}, {meh, moeder}]),
    
    teardown().
