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

-callback schema_info(pid()) ->
    persi:schema_info().

-callback table_info(persi:table(), pid()) ->
    persi_table:info().

-callback exec(persi:sql(), pid()) ->
    ok | {ok, RowsAffected :: non_neg_integer()}.

-callback flush_metadata(pid()) -> ok.


-export([start_driver/2, reg/1]).

-spec start_driver(persi:connection(), persi:connection_opts()) -> {ok, pid()}.
start_driver(Id, Opts) ->
    {driver, DriverModule} = proplists:lookup(driver, Opts),
    gen_server:start_link({via, gproc, {n, l, {persi_driver, Id}}}, DriverModule, Opts, []).


%% @doc MUST be called from the driver that is registering itself
reg(Module) ->
    gproc:reg({p, l, persi_driver_mod}, Module).
