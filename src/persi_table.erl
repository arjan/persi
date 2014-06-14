%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Operations on a single table

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

-module(persi_table).

-include_lib("persi.hrl").

-export(
   [
    insert/3,
    update/4,
    upsert/4,
    delete/3,
    select/3
   ]).


insert(_, _, _) ->
    ok.

update(_, _, _, _) ->
    ok.

upsert(_, _, _, _) ->
    ok.

delete(_, _, _) ->
    ok.

select(_, _, _) ->
    ok.

