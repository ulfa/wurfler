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
-include("../include/wurfler.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([import_wurflpatch/1]).
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
import_wurflpatch(Filename) ->
	Wurfl_Patch = xml_factory:parse_file(Filename),
	Devices = xmerl_xpath:string ("/wurfl_patch/devices/device", Wurfl_Patch),
	process_devices(Devices).

process_devices(Devices) ->
	[process_device(Device) || Device <- Devices].

process_device(Device) ->
	Id = xml_factory:get_attribute("/wurfl_patch/devices/device/@id", Device),
	case get_device(devicesTbl, Id) of
		[] -> wurfler_importer:process_device(Device);
		[DeviceDB] -> DeviceDB
	end,
	Groups = xml_factory:get_attribute("/wurfl/devices/device/@id", Device).

get_device(devicesTbl, Id) ->
	wurfler_db:find_record_by_id(devicesTbl, Id).

merge_device(DeviceXml, DeviceDb) ->
	ok.
merge_groups(GroupsXml, GroupsDb) ->
	ok.
merge_group(GroupXml, GroupDb) ->
	ok.
merge_capabilities(CapabilitiesXml, CapabilitiesDb) ->
	ok.
merge_capability(CapabilityXml, CapabilityDb) ->
	Value = xml_factory:get_attribute("/capability/@value", CapabilityXml),
	CapabilityDb#capability{value=Value}. 
	
get_capability(CapabilitiesDb, Capability_Name) ->
	[Capability || Capability <- CapabilitiesDb, Capability#capability.name==Capability_Name].

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
merge_capability_test() ->
	CapabilityDb = #capability{name="test", value="testDb"},
 	CapabilityXml = {xmlElement,capability,capability,[], 
					 {xmlNamespace,[],[]}, 
					 [{group,4},{device,6},{devices,2},{wurfl_patch,1}],4,
                   [{xmlAttribute,name,[],[],[],[],1,[],"test",false},
                    {xmlAttribute,value,[],[],[],[],2,[],"testXml",false}],
                   [],[],"./test",undeclared},
 	NewCapability = merge_capability(CapabilityXml, CapabilityDb),
	?assertEqual("testXml", NewCapability#capability.value).

get_capability_test() ->
	CapabilitiesDb = #capability{name="test", value="testDb"},
	[Cap] =  get_capability([CapabilitiesDb], "test"),
	?assertMatch(#capability{name="test", value="testDb"},Cap).
merge_device_test() ->
	ok.
%% 	merge_device(DeviceXml, DeviceDb).
import_wurflpatch_test()->
	ok.
get_device_test() ->
	?assertMatch([{device,"generic",[],undefined,"root",_,_,_}], get_device(devicesTbl, "generic")).
process_device_test() ->
	Wurfl_Patch = xml_factory:parse_file("./test/wurlfpatch.xml"),
	Devices = xmerl_xpath:string ("/wurfl_patch/devices/device", Wurfl_Patch),
	io:format("--- ~p~n", [Devices]).
	
