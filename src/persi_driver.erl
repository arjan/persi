%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Driver behaviour callbacks

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

-module(persi_driver).

-include("persi_int.hrl").

%% Return metadata information about the entire database
-callback schema_info(#persi_driver{}) ->
    persi:schema_info().

%% Return metadata information about the table
-callback table_info(persi:table(), #persi_driver{}) ->
    {ok, persi:table_info()} | {error, enotfound}.

%% Execute a plain SQL query
-callback exec(persi:sql(), #persi_driver{}) ->
    ok | {ok, RowsAffected :: non_neg_integer()}.

%% Flush any cached metadata in the driver process (called after a schema change)
-callback flush_metadata(#persi_driver{}) -> ok.

%% Full query, returns all rows plus column information
-callback q(persi:sql(), persi:sql_args(), #persi_driver{}) ->
    {ok, {persi:sql_result(), persi:column_names(), non_neg_integer()}} | persi:error().

-callback map_dialect(atom() | {atom(), term()}) ->
    term().

-callback acquire_connection(#persi_driver{}) -> #persi_driver{}.
-callback release_connection(#persi_driver{}) -> #persi_driver{}.

-export([start_driver/3, reg/1]).

-spec start_driver(persi:connection(), module(), persi:connection_opts()) -> {ok, pid()}.
start_driver(Id, DriverModule, Opts) ->
    gen_server:start_link({via, gproc, {n, l, {persi_driver, Id}}}, DriverModule, {Id, Opts}, []).


%% @doc MUST be called from the driver that is registering itself
reg(Module) ->
    gproc:reg({p, l, persi_driver_mod}, Module).
