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

-export(
   [
    q/2,
    q/3,
    transaction/1,
    transaction/2
   ]
  ).



-spec q(persi:sql(), persi:sql_args()) -> persi:q_result().
q(Sql, Args) ->
    %%io:format(user, ">> ~p~n", [iolist_to_binary(Sql)]),
    q(Sql, Args, ?PERSI_DEFAULT_CONNECTION).

-spec q(persi:sql(), persi:sql_args(), persi:connection() | #persi_driver{}) -> persi:q_result().
q(Sql, Args, Driver=#persi_driver{module=Mod}) ->
    Mod:q(Sql, Args, Driver);

q(Sql, Args, Connection) when is_atom(Connection) ->
    %%io:format(user, ">> ~p~n", [iolist_to_binary(Sql)]),
    Driver = persi_connection:lookup_driver(Connection),
    q(Sql, Args, Driver).


-spec transaction(fun()) -> term().
transaction(F) when is_function(F) ->
    transaction(F, ?PERSI_DEFAULT_CONNECTION).

-spec transaction(fun(), persi:connection() | #persi_driver{}) -> term().
transaction(F, #persi_driver{transaction=undefined, module=Mod}=Driver) ->
    Driver1 = Mod:acquire_connection(Driver),

    try
        Mod:exec("BEGIN", Driver1),
        Result = F(Driver1),
        Mod:exec("COMMIT", Driver1),
        Result
    catch
        Type:Reason ->
            Mod:exec("ROLLBACK", Driver1),
            case Type of
                throw ->
                    case Reason of
                        rollback -> {error, rollback};
                        _ -> throw(Reason)
                    end;
                error ->
                    error(Reason)
            end
    after
        Mod:release_connection(Driver1)
    end;

%% Nested transaction
transaction(F, #persi_driver{transaction=T}=Driver) when T =/= undefined ->
    F(Driver);

%% Entry point
transaction(F, Connection) when is_function(F) ->
    Driver = #persi_driver{module=Mod} = persi_connection:lookup_driver(Connection),
    true = Mod:map_dialect({check_support, transaction}),
    transaction(F, Driver).

