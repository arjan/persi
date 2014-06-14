%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Utilities

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

-module(persi_util).

-export([iolist_join/2]).


%% @doc Given a list of strings, create an iolist with the given separator.
-spec iolist_join([iolist()], term()) -> iolist().
iolist_join([], _) -> [];
iolist_join([X], _) -> [X];
iolist_join([First|Rest], Sep) ->
    lists:reverse(iolist_join(Rest, Sep, [First])).
iolist_join([], _Sep, Acc) ->
    Acc;
iolist_join([H|T], Sep, Acc) ->
    iolist_join(T, Sep, [H,Sep|Acc]).
