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
-module(wurfler_util).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([generate_etag/1, split_list/2]).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
generate_etag(Term) ->
	mochihex:to_hex(erlang:phash2(Term)).

split_list(List, 2) ->
	{L1, L2} = lists:split(length(List) div 2, List),
	[L1, L2];
split_list(List, 4) ->
	{L1, L2} = lists:split(length(List) div 2, List),
	{L3, L4} = lists:split(length(L1) div 2, L1),
	{L5, L6} = lists:split(length(L2) div 2, L2),
	[L3,L4,L5,L6].
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
