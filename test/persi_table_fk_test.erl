%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Test table related functions - foreign keys

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

-module(persi_table_fk_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("persi/include/persi.hrl").

-include("persi_eunit.hrl").

tables() ->
    persi:create_table(#persi_table{name=artist,
                                    columns=
                                        [
                                         #persi_column{name=id, type=int, notnull=true},
                                         #persi_column{name=name, type="varchar(50)", notnull=true}
                                        ],
                                    pk=[id]}),

    persi:create_table(#persi_table{name=track,
                                    columns=
                                        [
                                         #persi_column{name=id, type=int, notnull=true},
                                         #persi_column{name=artist_id, type=int, notnull=true},
                                         #persi_column{name=name, type="varchar(50)", notnull=true}
                                        ],
                                    pk=[id],
                                    fks=[#persi_fk{table=artist, from=artist_id, to=id}]
                                   }),
    
    ok.

    
full_test() ->
    setup(),
    tables(),
    
    ok = persi:insert(artist, [{id, 1}, {name, <<"Michael Jackson">>}]),

    {ok, Row} = persi:select(artist, 1),
    <<"Michael Jackson">> = proplists:get_value(name, Row),

    %% name may not be null
    {error, {constraint, _}} = persi:insert(track, [{id, 1}]), 

    %% artist id may not be null
    {error, {constraint, _}} = persi:insert(track, [{id, 1}, {name, <<"Heal the World">>}]),

    %% unknown artist id
    {error, {constraint, _}} = persi:insert(track, [{id, 1}, {artist_id, 666}, {name, <<"Heal the World">>}]),
  
    ok = persi:insert(track, [{id, 1}, {artist_id, 1}, {name, <<"Heal the World">>}]),
  
    teardown().
