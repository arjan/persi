%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Manages connection definitions

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

-module(persi_connection).

-export(
   [
    add/2,
    remove/1,
    driver_and_pid/1
   ]).

-spec add(persi:connection(), persi:connection_opts()) -> ok | {error, eexist}.
add(Connection, Opts) when is_atom(Connection), is_list(Opts) ->
    case is_pid(whereis_driver(Connection)) of
        true ->
            {error, eexist};
        false ->
            {ok, _Pid} = supervisor:start_child(persi_driver_sup, [Connection, Opts]),
            ok
    end.

-spec remove(persi:connection()) -> ok | {error, enotfound}.
remove(Connection) when is_atom(Connection) ->
    case whereis_driver(Connection) of
        undefined ->
            {error, enotfound};
        Pid when is_pid(Pid) ->
            ok = supervisor:terminate_child(persi_driver_sup, Pid)
    end.


driver_and_pid(Connection) ->
    case whereis_driver(Connection) of
        undefined ->
            throw({error, {unknown_persi_connection, Connection}});
        Pid ->
            {gproc:get_value({p, l, persi_driver_mod}, Pid), Pid}
    end.

whereis_driver(Connection) ->
    gproc:whereis_name({n, l, {persi_driver, Connection}}).
    

