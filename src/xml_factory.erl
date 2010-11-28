%% Author: ulf
%% Created: Nov 24, 2010
%% Description: TODO: Add description to xml_factory

%% Copyright 2010 Ulf Angermann
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
-module(xml_factory).

%%
%% Include files
%%
-include("../include/wurfler.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").
%%
%% Exported Functions
%%
-export([create_xml/2, parse/1, get_attribute/2]).

%%
%% API Functions
%%
create_xml(devices, Devices)->
	[create_xml(device, Device) || Device <- Devices];
create_xml(device, #device{id=Id, user_agent=U_A, actual_device_root=A_D_R, fall_back=F_B, groups=Groups})->
	{'device', [{id, Id}, {user_agent, U_A}, {actual_device_root, A_D_R}, {fall_back, F_B}],
	 create_xml(groups, Groups)};
create_xml(groups, Groups)->
	[create_xml(group, Group) || Group <- Groups];
create_xml(capabilities, Capabilities)->
	[create_xml(capability, Cap) || Cap <- Capabilities];
create_xml(group, #group{id=Id, capabilites=Capabilites})->
	Caps = create_xml(capabilities, Capabilites),
	{'group', [{id, Id}], Caps};
create_xml(capability, Capability)->
	{'capability', [{name, Capability#capability.name}, {value, Capability#capability.value}], []}.

parse(Bin) when is_binary(Bin) ->
	{Xml,_} = xmerl_scan:string(binary_to_list(Bin)),
	Xml;

parse(String) when is_list(String)->
	{Xml,_} = xmerl_scan:string(String),
	Xml.
get_attribute(XPath, Node) ->
	case xmerl_xpath:string(XPath, Node) of
		[#xmlAttribute{value = Value}] -> Value;
		O -> O
	end.
%%
%% Local Functions
%%
%%
%% Test Functions
%%
create_xml_capability_test() ->
	Cap = #capability{name="myVersion", value="1.0"},
	log_xml([create_xml(capability, Cap)]),
	?assertEqual({capability,[{name,"myVersion"},{value,"1.0"}],[]}, create_xml(capability, Cap)).

create_xml_capabilities_test() ->
	Caps = create_capabilities_1(),
	?assertEqual(2, erlang:length(Caps)).

create_xml_group_test() ->
	G=create_xml(group, create_group_1()),
 	?assertEqual("j2me", get_attribute("/group/@id", parse(tto_xml([G])))).

create_xml_device_test() ->
	Device = create_device(),
	D=create_xml(device, Device),
	log_xml([D]).



create_device()->
	#device{id="Nokia", user_agent="blahblahblah", actual_device_root=undefined, fall_back=undefined, 
			groups=[create_group_1(), create_group_2()]}.

create_goups() ->
	[create_group_1(), create_group_2()].
create_group_1() ->
	#group{id="j2me", capabilites=create_capabilities_1()}.
create_group_2() ->
	#group{id="html", capabilites=create_capabilities_2()}.

create_capabilities_1() ->
	[
	#capability{name="myVersion", value="1.0"},
	#capability{name="myProfile", value="2.0"}
	].

create_capabilities_2() ->
	[
	#capability{name="myVersion1", value="1.0"},
	#capability{name="myProfile1", value="2.0"}
	].

tto_xml(A)->
	lists:flatten(xmerl:export_simple_content(A, xmerl_xml)).


log_xml(Element)->
	B=lists:flatten(xmerl:export_simple_content(Element, xmerl_xml)),
	io:format("Xml: ~n~p~n", [B]).
	

