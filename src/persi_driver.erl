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

%% @doc Return metadata information about the entire database
-callback schema_info(#persi_driver{}) ->
    persi:schema_info().

%% @doc Return metadata information about the table
-callback table_info(persi:table(), #persi_driver{}) ->
    {ok, persi:table_info()} | {error, enotfound}.

%% @doc Execute a plain SQL query
-callback exec(persi:sql(), #persi_driver{}) ->
    ok | {ok, RowsAffected :: non_neg_integer()}.

%% @doc Flush any cached metadata in the driver process (called after a schema change)
-callback flush_metadata(#persi_driver{}) -> ok.

%% @doc Full query, returns all rows plus column information
-callback fetchall(persi:sql(), persi:sql_args(), #persi_driver{}) ->
    {ok, {persi:sql_result(), persi:column_names(), non_neg_integer()}} | persi:error().


-export([start_driver/2, reg/1]).

-spec start_driver(persi:connection(), persi:connection_opts()) -> {ok, pid()}.
start_driver(Id, Opts) ->
    {driver, DriverModule} = proplists:lookup(driver, Opts),
    Opts1 = proplists:delete(driver, Opts),
    gen_server:start_link({via, gproc, {n, l, {persi_driver, Id}}}, DriverModule, {Id, Opts1}, []).


%% @doc MUST be called from the driver that is registering itself
reg(Module) ->
    gproc:reg({p, l, persi_driver_mod}, Module).
