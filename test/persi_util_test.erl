%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Test utility functions

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

-module(persi_util_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("persi/include/persi.hrl").

iolist_join_test() ->

    [] = persi_util:iolist_join([], $,),

    <<"foo,bar">> = iolist_to_binary(persi_util:iolist_join(["foo", "bar"], $,)),

    <<"foo :: bar">> = iolist_to_binary(persi_util:iolist_join(["foo", "bar"], " :: ")),

    ok.


map_sql_column_generic_test() ->
    #persi_column{type=varchar, length=61} = persi_util:map_sql_to_column(<<"varchar(61)">>),
    #persi_column{type=varchar, length=161} = persi_util:map_sql_to_column(<<"varchar(161)">>),
    #persi_column{type=varchar, length=11} = persi_util:map_sql_to_column(<<"VARCHAR(11)">>),
    undefined = persi_util:map_sql_to_column(<<"xxvarchar(61)">>),
    ok.

