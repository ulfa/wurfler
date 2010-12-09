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

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/wurfler.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).
-export([import_wurfl_patch/1]).
%% ====================================================================
%% External functions
%% ====================================================================
import_wurfl_patch(Filename) ->
	gen_server:cast(?MODULE, {import_wurfl_patch, Filename}).


%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {generic}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
	start_link().
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({import_wurflpatch, Filename}, State) ->
	Generic = get_device(devicesTbl, "generic"),
	import_wurflpatch(Filename, State#state{generic=Generic}),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
import_wurflpatch(Filename, State) ->
	Wurfl_Patch = xml_factory:parse_file(Filename),	
	Devices = xmerl_xpath:string ("/wurfl_patch/devices/device", Wurfl_Patch),
	process_devices(Devices, State).

process_devices(Devices, State) ->
	[process_device(Device, State) || Device <- Devices].

process_device(DeviceXml, State) ->
	Id = xml_factory:get_attribute("/wurfl_patch/devices/device/@id", DeviceXml),
	case get_device(devicesTbl, Id) of
		[] -> wurfler_importer:process_device(DeviceXml);
		[DeviceDB] -> merge_device(DeviceXml, DeviceDB)
	end.

get_device(devicesTbl, Id) ->
	wurfler_db:find_record_by_id(devicesTbl, Id).

get_group_of_device(Device, Group_Id) ->
	[Group || Group <- Device#device.groups, Group#group.id == Group_Id].

get_group_of_groups(Groups, Group_Id) ->
	[Group || Group <- Groups, Group#group.id == Group_Id].
	
get_capability_from_group(Group, Capability_Name) ->
	[Capability || Capability <- Group#group.capabilites, Capability#capability.name == Capability_Name].

merge_device(DeviceXml, DeviceDb) ->
	GroupsXml = xmerl_xpath:string ("/wurfl_patch/devices/device/group", DeviceXml),
	Groups = merge_groups(GroupsXml,DeviceDb#device.groups),
	User_Agent = xml_factory:get_attribute("/wurfl_patch/devices/device/@user_agent", DeviceXml),
	Actual_device_root = xml_factory:get_attribute("/wurfl_patch/devices/device/@actual_device_root", DeviceXml),
	Fall_back = xml_factory:get_attribute("/wurfl_patch/devices/device/@fall_back", DeviceXml),
	DeviceDb#device{user_agent=User_Agent, actual_device_root=Actual_device_root, fall_back=Fall_back, groups=Groups}.

merge_groups(GroupsXml, GroupsDb) ->
	[merge_group(GroupXml, GroupsDb) || GroupXml <- GroupsXml].
	
merge_group(GroupXml, GroupDb) ->
	Id = xml_factory:get_attribute("/group/@id", GroupXml),
	case get_group_of_groups(GroupDb, Id) of
		[] ->  wurfler_importer:process_group(GroupXml);		
		[Group] -> io:format("1... ~n "),
					CapsXml = xmerl_xpath:string("/group/capability", GroupXml),
				   io:format("2... ~p~n ", [CapsXml]),
				   Caps = merge_capabilities(CapsXml, Group#group.capabilites),
				   Group#group{capabilites=Caps}	
	end.

merge_capabilities(CapabilitiesXml, CapabilitiesDb) ->
	[merge_capability(CapabilityXml, CapabilitiesDb) || CapabilityXml <- CapabilitiesXml].

merge_capability(CapabilityXml, CapabilitiesDb) ->
	Name = xml_factory:get_attribute("/capability/@name", CapabilityXml),
	Value = xml_factory:get_attribute("/capability/@value", CapabilityXml),
	case get_capability(CapabilitiesDb, Name) of 
		[] -> wurfler_importer:process_capability(CapabilityXml);
		[Capability] -> Capability#capability{value=Value}
	end.
	
get_capability(CapabilitiesDb, Capability_Name) ->
	[Capability || Capability <- CapabilitiesDb, Capability#capability.name == Capability_Name].
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
merge_group_test() ->
	GroupXml = xml_factory:parse_file("./test/group_xml"),
	Group = xmerl_xpath:string ("/device/group", GroupXml),
	{ok, [GroupsDb]} = file:consult("./test/group_patch"),
	io:format("3... ~n "),
	?assertMatch({group, "magical_powers", _},merge_group(Group, GroupsDb)).

merge_groups_test() ->
	DeviceXml = xml_factory:parse_file("./test/group_patch_xml"),
	GroupsXml = xmerl_xpath:string ("/device/group", DeviceXml),
	{ok, [GroupsDb]} = file:consult("./test/group_patch"),
	?assertEqual(3,erlang:length(merge_groups(GroupsXml, GroupsDb))).
	
merge_capability_test() ->
	CapabilitiesDb = [#capability{name="test", value="testDb"}],
 	CapabilityXml = {xmlElement,capability,capability,[], 
					 {xmlNamespace,[],[]}, 
					 [{group,4},{device,6},{devices,2},{wurfl_patch,1}],4,
                   [{xmlAttribute,name,[],[],[],[],1,[],"test1",false},
                    {xmlAttribute,value,[],[],[],[],2,[],"testXml",false}],
                   [],[],"./test",undeclared},
 	NewCapability = merge_capability(CapabilityXml, CapabilitiesDb),
	?assertEqual("test1", NewCapability#capability.name),
	?assertEqual("testXml", NewCapability#capability.value).

get_capability_test() ->
	CapabilitiesDb = #capability{name="test", value="testDb"},
	[Cap] =  get_capability([CapabilitiesDb], "test"),
	?assertMatch(#capability{name="test", value="testDb"},Cap).

get_group_of_device_test() ->
  [Device] = get_device(devicesTbl, "generic"),
  [Group]=get_group_of_device(Device, "j2me"),
  ?assertEqual("j2me", Group#group.id).

get_capability_from_group_test() ->
	[Device] = get_device(devicesTbl, "generic"),
	[Group]=get_group_of_device(Device, "j2me"),
	[Cap] = get_capability_from_group(Group, "j2me_canvas_width"),
	?assertEqual("j2me_canvas_width", Cap#capability.name).

merge_device_test() ->
	ok.
%% 	merge_device(DeviceXml, DeviceDb).
import_wurflpatch_test()->
	ok.
get_device_test() ->
	?assertMatch([{device,"generic",[],undefined,"root",_,_,_}], get_device(devicesTbl, "generic")).
process_device_test() ->
	Wurfl_Patch = xml_factory:parse_file("./test/wurlfpatch.xml"),
	Devices = xmerl_xpath:string ("/wurfl_patch/devices/device", Wurfl_Patch).
