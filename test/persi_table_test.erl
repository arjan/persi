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


column_type_datetime_test() ->
    setup(),
    Table = #persi_table{name=demotable,
                         columns=
                             [
                              #persi_column{name=id, type=int},
                              #persi_column{name=modified, type=datetime, notnull=true},
                              ?persi_props_column
                             ]
                        },
    persi:create_table(Table),

    Date = {{2014, 1, 1}, {0,0,0}},
    ok = persi:insert(demotable, [{id, 1}, {modified, Date}]),

    %% check that date is returned properly
    {ok, {[[{{2014, 1, 1}, {0,0,_}}]], _, _}} = persi:q("SELECT modified FROM demotable", []),

    {ok, 1} = persi:update(demotable, 1, [{modified, {{2014, 1, 2}, {0,0,0}} }]),
    {ok, {[[{{2014, 1, 2}, {0,0,_}}]], _, _}} = persi:q("SELECT modified FROM demotable", []),

    {ok, 1} = persi:update(demotable, 1, [{modified, {{2014, 1, 3}, {0,0,0}} }]),
    {ok, {[[{{2014, 1, 3}, {0,0,_}}]], _, _}} = persi:q("SELECT modified FROM demotable", []),

    teardown().


default_current_timestamp_test() ->

    case os:getenv("DBDRIVER") of
        "mysql" ->
            %% current_timestamp on datetime columns not supported on MySQL < 5.6.0
            skip;

        _ ->

            setup(),
            Table = #persi_table{name=demotable,
                                 columns=
                                     [
                                      #persi_column{name=id, type=varchar, length=255, notnull=true},
                                      #persi_column{name=created, type=datetime, notnull=true, default=current_timestamp}
                                     ]
                                },
            persi:create_table(Table),

            ok = persi:insert(demotable, [{id, "Hello"}]),

            %% check that a current timestamp is inserted
            {ok, {[[{{_,_,_}, {_,_,_}}]], _, _}} = persi:q("SELECT created FROM demotable", []),

            teardown()
    end.


escaping_test() ->
    setup(),

    case os:getenv("DBDRIVER") of
        "mysql" ->
            %% weirdness in emysql
            skip;

        _ ->
            Table = #persi_table{name=xtable,
                                 columns=
                                     [
                                      #persi_column{name=xorder, type=varchar, length=255, notnull=true},
                                      #persi_column{name=xdesc, type=datetime, notnull=true, default=current_timestamp}
                                     ]
                                },
            ok = persi:create_table(Table)
            end,
    teardown().

manycolumns_test() ->
    setup(),
    Columns = [a,b,c,d,e,f,g,h,i,j,k,l,m,n],
    Table = #persi_table{name=atable,
                         columns=
                             [#persi_column{name=C, type=varchar, length=10} || C <- Columns]
                        },
    ok = persi:create_table(Table),

    Values = lists:zip(Columns, [<<"foo">> || _ <- Columns]),
    ok = persi:insert(atable, Values),

    teardown().


column_type_float_test() ->
    setup(),
    Table = #persi_table{name=demotable,
                         columns=
                             [
                              #persi_column{name=id, type=int},
                              #persi_column{name=value, type=float}
                             ]
                        },
    persi:create_table(Table),

    FloatValue = 3.1415,

    ok = persi:insert(demotable, [{id, 1}, {value, FloatValue}]),

    %% check that date is returned properly
    {ok, {[[FloatValue]], _, _}} = persi:q("SELECT value FROM demotable", []),

    FloatValue2 = FloatValue * FloatValue, % rangom
    
    {ok, 1} = persi:update(demotable, 1, [{value, FloatValue2 }]),
    {ok, {[[FloatValue2]], _, _}} = persi:q("SELECT value FROM demotable", []),

    teardown().
