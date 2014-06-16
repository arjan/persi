%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Query functions

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

-module(persi_query).

-include_lib("persi/include/persi.hrl").
-include("persi_int.hrl").

-export([q/2, q/3]).

-export_type([q_result/0]).

-type q_result() ::
        {ok, {persi:sql_result(), persi:column_names(), non_neg_integer()}} | persi:error().        

-spec q(persi:sql(), persi:sql_args()) -> q_result().
q(Sql, Args) ->
    q(Sql, Args, ?PERSI_DEFAULT_CONNECTION).

-spec q(persi:sql(), persi:sql_args(), persi:connection()) -> {ok, {persi:sql_result(), persi:column_names(), non_neg_integer()}} | persi:error().
q(Sql, Args, Connection) ->
    Driver = #persi_driver{module=Mod} = persi_connection:lookup_driver(Connection),
    Mod:q(Sql, Args, Driver).

