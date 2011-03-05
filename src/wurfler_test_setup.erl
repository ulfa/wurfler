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
-module(wurfler_test_setup).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("../include/wurfler.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([setup/0, teardown/0]).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
setup() ->
	wurfler:start(),
	wurfler_search:start(),
	mnesia:start(),
	mnesia:wait_for_tables([devicesTbl, brand_index, capabilities_devices, changed_caps_devices, capability_description, os_device_id], 3000).
%%	mnesia:clear_table(devicesTbl),
%%	mnesia:clear_table(brand_index).

teardown() ->
	wurfler:terminate([], []),
	mnesia:stop().
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
