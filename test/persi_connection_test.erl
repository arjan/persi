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

-module(persi_connection_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("persi/include/persi.hrl").

-define(DBFILE, ":memory:").

setup() ->
    application:start(gproc),
    application:start(persi),
    ok.

dup_default_connection_test() ->
    application:start(gproc),
    application:start(persi),

    ok = persi:add_connection([{driver, persi_driver_esqlite}, {dbfile, ?DBFILE}]),
    {error, eexist} = persi:add_connection([]),

    ok = persi:remove_connection(),
    ok.

dup_named_connection_test() ->
    application:start(gproc),
    application:start(persi),

    ok = persi:add_connection(sqlite1, [{driver, persi_driver_esqlite}, {dbfile, ?DBFILE}]),
    ok = persi:add_connection(sqlite2, [{driver, persi_driver_esqlite}, {dbfile, "/tmp/test1.db"}]),
    
    {error, eexist} = persi:add_connection(sqlite1, []),
    ok = persi:remove_connection(sqlite1),
    ok = persi:remove_connection(sqlite2),
    ok.


add_remove_connection_test() ->
    application:start(gproc),
    application:start(persi),

    {error, enotfound} = persi:remove_connection(asdf),
    
    ok = persi:add_connection(asdf, [{driver, persi_driver_esqlite}, {dbfile, ?DBFILE}]),
    {error, eexist} = persi:add_connection(asdf, [{driver, persi_driver_esqlite}, {dbfile, ?DBFILE}]),

    ok = persi:remove_connection(asdf),

    ok = persi:add_connection(asdf, [{driver, persi_driver_esqlite}, {dbfile, ?DBFILE}]),

    ok = persi:remove_connection(asdf),
    ok.
