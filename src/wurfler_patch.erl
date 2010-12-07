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
-module(wurfler_patch).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([import_wurflpatch/1]).
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
import_wurflpatch(Filename) ->
	Wurfl_Patch = xml_factory:parse_file(Filename),
	Devices = xmerl_xpath:string ("/wurfl/devices/device", Wurfl_Patch),
	process_devices(Devices).

process_devices(Devices) ->
	[process_device(Device) || Device <- Devices].

process_device(Device) ->
	Id = xml_factory:get_attribute("/wurfl/devices/device/@id", Device),
	Groups = xml_factory:get_attribute("/wurfl/devices/device/@id", Device).

get_device(devicesTbl, Id) ->
	case wurfler_db:find_record_by_id(devicesTbl, Id) of
		[] -> [];
		[Device] -> Device 
	end.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
import_wurflpatch_test()->
	ok.
get_device_test() ->
	?assertEqual([], get_device(devicesTbl, "unknown")),
	?assertMatch({device,"generic",[],undefined,"root",_,_,_}, get_device(devicesTbl, "generic")).
