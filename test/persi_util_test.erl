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


iolist_join_test() ->

    [] = persi_util:iolist_join([], $,),

    <<"foo,bar">> = iolist_to_binary(persi_util:iolist_join(["foo", "bar"], $,)),

    <<"foo :: bar">> = iolist_to_binary(persi_util:iolist_join(["foo", "bar"], " :: ")),

    ok.
