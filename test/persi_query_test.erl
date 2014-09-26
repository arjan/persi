%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Test schema related functions

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

-module(persi_query_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("persi/include/persi.hrl").

-include("persi_eunit.hrl").

-define(DBFILE, ":memory:").


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


rows_test() ->
    setup(),
    demotable(),

    ok = persi:insert(demotable, [{id, 1}, {name, "Piet"}]),
    ok = persi:insert(demotable, [{id, 2}, {name, "Jan"}]),

    Rows = persi:rows(demotable, "SELECT * FROM demotable", []),

    [
     [{id, 1}, {name, <<"Piet">>}],
     [{id, 2}, {name, <<"Jan">>}]
    ] = Rows,

    teardown().
