%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Include file with exported records

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

-record(persi_schema, {tables=[]}).

-record(persi_column, {name,
                       type,
                       default=undefined,
                       notnull=false}).

-record(persi_table, {name, columns=[], pk=[], fks=[], has_props=false}).

-record(persi_fk, {table, from, to}).

-define(persi_props_column_name, props).
-define(persi_props_column, #persi_column{name=?persi_props_column_name, type=blob, notnull=true}).
