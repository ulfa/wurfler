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
-module(wurfler_SUITE).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").
-define(HTML_CONTENT_TYPE, [{"Content-Type", "text/html"}, {"Accept", "text/html"}]).
-define(XML_CONTENT_TYPE, [{"Content-Type", "text/xml"}, {"Accept", "text/xml"}]).
%%-define(HOST, "http://wurfler.dyndns.org:8000").
-define(HOST, "http://localhost:8000").
%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 20}}].

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() -> [ {search_resource, [sequence], [search_by_caps]},
			 {device_get_requests, [sequence], [get_device_by_id, get_device_by_id_404, get_device_by_ua]},
			 {device_get_requests_html, [sequence], [get_device_by_id_to_html, get_device_by_ua_to_html, get_device_by_id_404_to_html]},
			 {devices_post_requests, [sequence], [post_cap_query_no_caps, post_cap_query, post_cap_query_with_timestamp,
												  post_cap_query_device_os_version, post_cap_android]},
			 {brand_get_requests, [sequence], [get_brand_by_brand_name, get_all_brands, get_brand_by_brand_name_to_html]},
			 {brand_get_requests_html, [sequence], [get_brand_by_brand_name_to_html]},
			 {model_get_requests, [sequence], [get_devices_by_model_name]},
			 {model_get_requests_html, [sequence], [get_devices_by_model_name_html]},
			 {device_delete, [sequence] , [delete_device_by_id]},
			 {changes_resource, [sequence] , [find_changes]}
			].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() -> [{group, device_get_requests}, {group, device_get_requests_html}, {group, devices_post_requests},
		  {group, brand_get_requests}, {group, brand_get_requests_html},{group, model_get_requests},
		  {group, model_get_requests_html},
		  {group, changes_resource}
		 ].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) -> 
	ibrowse:start(),
	Config.
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
	ibrowse:stop(),
	ok.
%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(_group, Config) ->
	Config.
%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(_group, Config) ->
	Config.
%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    Config.

get_device_by_id(_Config)->
	{ok, "200", _C, _D}=ibrowse:send_req(?HOST ++ "/device/htc_desirehd_ver1_subtelus", ?XML_CONTENT_TYPE, get).
get_device_by_id_to_html(_Config)->
	{ok, "200", _C, _D}=ibrowse:send_req(?HOST ++ "/device/generic", ?HTML_CONTENT_TYPE, get).
get_device_by_id_404(_Config) ->
	{ok, "404", _C, _D}=ibrowse:send_req(?HOST ++ "/device/unknown", ?XML_CONTENT_TYPE, get).
get_device_by_id_404_to_html(_Config) ->
	{ok, "404", _C, _D}=ibrowse:send_req(?HOST ++ "/device/unknown", ?HTML_CONTENT_TYPE, get).

get_device_by_ua(_Config) ->
	{ok, "200", _C, _D}=ibrowse:send_req(?HOST ++ "/device", [{"Content-Type", "text/xml"}, {"User-Agent", "Mozilla/5.0 (Linux; U; Android 1.1; en-us; T-Mobile G1 Build/PLAT-RC33) AppleWebKit/525.10+ (KHTML, like Gecko) Version/3.0.4 Mobile Safari/523.12.2"}], get).
get_device_by_ua_to_html(_Config) ->
	{ok, "200", _C, _D}=ibrowse:send_req(?HOST ++ "/device", [{"Content-Type", "text/html"}, {"Accept", "text/html"}, {"User-Agent", "Mozilla/5.0 (Linux; U; Android 1.1; en-us; T-Mobile G1 Build/PLAT-RC33) AppleWebKit/525.10+ (KHTML, like Gecko) Version/3.0.4 Mobile Safari/523.12.2"}], get).

search_by_caps(_Config) ->
	A="<?xml version=\"1.0\" encoding=\"utf-8\"?><query key=\"1110\"><capabilities><capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"=\"/><capability name=\"j2me_midp_1_0\" value=\"true\" operator=\"=\"/></capabilities></query>",
	{ok, "200", _C, D}=ibrowse:send_req(?HOST ++ "/search", ?XML_CONTENT_TYPE, post, A).

%% Tests for the device service (POST)
post_cap_query(_Config) ->
	A="<?xml version=\"1.0\" encoding=\"utf-8\"?><query key=\"1110\"><capabilities><capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"=\"/><capability name=\"j2me_midp_1_0\" value=\"true\" operator=\"=\"/></capabilities></query>",
	{ok, "200", _C, D}=ibrowse:send_req(?HOST ++ "/devices", ?XML_CONTENT_TYPE, post, A).
%%	Xml = xml_factory:parse(D),
%%	Devices = xmerl_xpath:string("//devices/device", Xml),
%%	io:format("Anzahl : ~p~n", [erlang:length(Devices)]),
%%	1743=erlang:length(Devices).


post_cap_query_no_caps(_Config) ->
	A="<?xml version=\"1.0\" encoding=\"utf-8\"?><query><capabilities/></query>",
	{ok, "200", _C, Body}=ibrowse:send_req("http://localhost:8000/devices", ?XML_CONTENT_TYPE, post, A),
	"<devices/>" == Body.

post_cap_query_with_timestamp(_Config) ->
	A="<?xml version=\"1.0\" encoding=\"utf-8\"?><query key=\"1110\"><timestamp>01.01.2010</timestamp><capabilities><capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"=\"/><capability name=\"j2me_midp_1_0\" value=\"true\" operator=\"=\"/></capabilities></query>",
	{ok, "200", _C, D}=ibrowse:send_req(?HOST ++ "/devices", ?XML_CONTENT_TYPE, post, A).
%%	Xml = xml_factory:parse(D),
%%	Devices = xmerl_xpath:string("//devices/device", Xml),
%%	1743=erlang:length(Devices).

post_cap_query_with_type(_Config) ->
	A="<?xml version=\"1.0\" encoding=\"utf-8\"?><query key=\"1110\" ><capabilities><capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"=\"/><capability name=\"j2me_midp_1_0\" value=\"true\" operator=\"=\"/></capabilities></query>",
	{ok, "200", _C, D}=ibrowse:send_req(?HOST ++ "/devices", ?XML_CONTENT_TYPE, post, A).

post_cap_query_device_os_version(_Config) ->
	A="<?xml version=\"1.0\" encoding=\"utf-8\"?><query key=\"1111\"><timestamp>01.01.2010</timestamp><capabilities><capability name=\"device_os\" value=\"iPhone OS\" operator=\"=\"/><capability name=\"device_os_version\" value=\"0.0\" operator=\">\"/></capabilities></query>",
	{ok, "200", _C, D}=ibrowse:send_req(?HOST ++ "/devices", ?XML_CONTENT_TYPE, post, A).
%%	Xml = xml_factory:parse(D),
%%	Devices = xmerl_xpath:string("//devices/device", Xml),	
%%	4= erlang:length(Devices).
	
%% Tests for the brand service
get_brand_by_brand_name(_Config) ->
	{ok, "200", _C, D}=ibrowse:send_req(?HOST ++ "/brand/RIM", ?XML_CONTENT_TYPE, get),
	Brand = xml_factory:parse(D),
	"RIM" = xml_factory:get_attribute("//brand/@name", Brand).
get_brand_by_brand_name_to_html(_Config) ->
	{ok, "200", _C, _D}=ibrowse:send_req(?HOST ++ "/brand/RIM",?HTML_CONTENT_TYPE, get).
get_all_brands(_Config) ->
	{ok, "200", _C, _D}=ibrowse:send_req(?HOST ++ "/brands", ?XML_CONTENT_TYPE, get).
%% Tests for the model service
get_devices_by_model_name(_Config) ->
	{ok, "200", _C, _D}=ibrowse:send_req(?HOST ++ "/device?model=MB200", ?XML_CONTENT_TYPE, get).
get_devices_by_model_name_html(_Config) ->
	{ok, "200", _C, _D}=ibrowse:send_req(?HOST ++ "/device?model=MB200", ?HTML_CONTENT_TYPE, get).

delete_device_by_id(_Config) ->
	{ok, "204", _C, D}=ibrowse:send_req(?HOST ++ "/device/ahong_d13_ver1", ?XML_CONTENT_TYPE, delete).

find_changes(_Config) ->
		{ok, "200", _C, D}=ibrowse:send_req(?HOST ++ "/changes/01012010", ?XML_CONTENT_TYPE, get),
		io:format("1.. ~p~n", [D]).

post_cap_android(_Config) ->
	A="<?xml version=\"1.0\" encoding=\"utf-8\"?><query key=\"1111\" type=\"Android\"><capabilities><capability name=\"device_os\" value=\"Android\" operator=\"=\"/></capabilities></query>",
	{ok, "200", _C, D}=ibrowse:send_req(?HOST ++ "/devices", ?XML_CONTENT_TYPE, post, A).
%%	Xml = xml_factory:parse(D),
%%	Devices = xmerl_xpath:string("//devices/device", Xml),	
%%	186 = erlang:length(Devices).	