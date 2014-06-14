%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Functions relating to schema information

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

-module(persi_schema).

-export([info/1, table_info/2]).
-export_type([info/0, table_info/0]).

-opaque info() :: [table_info()].
-opaque table_info() :: [table_info()].

info(Connection) ->
    {Mod, Pid} = persi_connection:driver_and_pid(Connection),
    Mod:schema_info(Pid).

table_info(Table, Connection) ->
    {Mod, Pid} = persi_connection:driver_and_pid(Connection),
    Mod:table_info(Table,Pid).

