%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Selections of all kinds

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

-module(persi_transaction_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("persi/include/persi.hrl").

-include("persi_eunit.hrl").

transaction_ok_test() ->
    setup(),

    case os:getenv("DBDRIVER") of
        "mysql" ->
            %% Transactions not (yet) supported on MySQL
            skip;

        _ ->

            ok = persi:transaction(
                   fun(_C) ->
                           ok
                   end
                  ),

            {foo,bar} = persi:transaction(
                          fun(_C) ->
                                  {foo,bar}
                          end
                         )
    end,

    teardown().

transaction_fail_test() ->
    setup(),

    case os:getenv("DBDRIVER") of
        "mysql" ->
            %% Transactions not (yet) supported on MySQL
            skip;

        _ ->

            {error, rollback} = persi:transaction(
                                  fun(_C) ->
                                          throw(rollback)
                                  end
                                 ),

            {'EXIT', {{badmatch, _}, _}}
                = (catch
                       persi:transaction(
                         fun(_C) ->
                                 1=2
                         end
                        ))
    end,

    teardown().



nested_transaction_test() ->
    setup(),


    case os:getenv("DBDRIVER") of
        "mysql" ->
            %% Transactions not (yet) supported on MySQL
            skip;

        _ ->

            {ok} = persi:transaction(
                     fun(D) ->
                             persi:transaction(
                               fun(_) ->
                                       {ok}
                               end,
                               D)
                     end
                    ),

            {error, rollback} = persi:transaction(
                                  fun(D) ->
                                          persi:transaction(
                                            fun(D1) ->
                                                    {ok, {[[1]], _, _}} = persi:q("SELECT 1", [], D1),
                                                    throw(rollback)
                                            end,
                                            D)
                                  end
                                 ),


            {'EXIT', {{badmatch, _}, _}}
                = (catch
                       persi:transaction(
                         fun(D) ->
                                 persi:transaction(
                                   fun(_) ->
                                           1=2
                                   end,
                                   D)
                         end
                        ))
    end,

    teardown().
