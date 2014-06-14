%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Example for eunit on Persi schema migrations

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

-module(schema_migrations_example).

-behaviour(persi_schema).

-include_lib("persi/include/persi.hrl").


-export(
   [
    schema_version/0,
    manage/2
   ]).

schema_version() -> 1.

manage(install, Connection) ->
    persi:create_table(#persi_table{name=test, columns=[#persi_column{name=id, type=int, notnull=true}], pk=[id]}, Connection).

     



