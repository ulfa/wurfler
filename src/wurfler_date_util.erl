%% Copyright 2010 Ulf
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

%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(wurfler_date_util).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([get_uc_time/0, parse_to_datetime/1]).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
get_uc_time() ->
	erlang:universaltime().

parse_to_datetime(DateString) ->
	Parse = fun (Start, Length) -> {I, _} = string:to_integer(string:substr(DateString, Start, Length)), I end,
	{{Parse(7, 4), Parse(4, 2), Parse(1, 2)}, {0, 0, 0}}.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
parse_to_datetime_test() ->
	?assertEqual({{2010, 12, 19}, {00,00,00}},parse_to_datetime("19.12.2010")).