%% Author: ulf
%% Created: Nov 23, 2010
%% Description: TODO: Add description to devices_resource

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
-module(devices_resource).

%%
%% Include files
%%
-include("../include/wurfler.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").


%%
%% Exported Functions
%%
-export([init/1, to_xml/2, to_html/2, post_is_create/2, content_types_provided/2, process_post/2]).
-compile([export_all]).
-include_lib("../deps/webmachine/include/webmachine.hrl").
-record(context, {devices=[]}).

%%
%% API Functions
%%
init([]) -> 
	{ok, #context{devices=[]}}.

content_types_provided(ReqData, Context) ->
    {[{"text/xml", to_xml}, {"text/html", to_html}],ReqData, Context}.


allowed_methods(ReqData, Context) ->
    {['POST', 'GET'], ReqData, Context}.

to_xml(ReqData, #context{devices=Devices}=Context)->
	Content = xml_factory:to_xml(Devices),
	{Content, ReqData, Context}.

to_html(ReqData, #context{devices=Devices}=Context) ->
	{ok, Content} = devices_dtl:render([{devices, Devices}]),
     {Content, ReqData, Context}.

allow_missing_post(ReqData, Context) ->
	{true, ReqData, Context}.

post_is_create(ReqData, Context) ->
	{false, ReqData, Context}.

resource_exists(ReqData, Context) ->
	case get_devices_by_model(wrq:path_info(model, ReqData)) of
		[] -> {false, ReqData, Context#context{devices=[]}};
		Devices -> {true, ReqData, Context#context{devices=Devices}}
	end.

process_post(ReqData, Context) ->
	Body = wrq:req_body(ReqData),
	Caps = get_capabilities(Body),
	Key = get_key(Body),
	Timestamp = get_timestamp(Body),
	Devices = get_devices(Caps, Timestamp),
	%%save_caps_devices(Caps, Devices, Key),
	D = xml_factory:to_xml(Devices),
	{true, wrq:append_to_response_body(D, ReqData), Context}.
%%
%% Local Functions
%%
get_key(Body) ->
	Xml = xml_factory:parse(Body),
	xml_factory:get_attribute("/query/@key", Xml).
get_capabilities(Body) ->
	Xml = xml_factory:parse(Body),
	Caps = xmerl_xpath:string ("/query/capabilities/capability", Xml),
	[create_cap(Cap)||Cap <- Caps].	

get_devices([], _Timestamp) ->
	xml_factory:create_devices([]);
get_devices(Capabilities, []) ->
	wurfler:searchByCapabilities(Capabilities, ?DEFAULT_TIMESTAMP);
get_devices(Capabilities, Timestamp) ->
	wurfler:searchByCapabilities(Capabilities, Timestamp).

get_devices_by_model(Model_Name) ->
	wurfler:get_devices_by_model(Model_Name).

save_caps_devices([], _Devices, _Key) ->
	ok;
save_caps_devices(Caps, Devices, Key) ->
	wurfler_cache:save_caps_devices(Caps, Devices, Key).

get_timestamp(Body) ->
	Xml = xml_factory:parse(Body),
	xml_factory:get_text("/query/timestamp/text()", Xml).

create_cap(Cap) ->
	Name = xml_factory:get_attribute("/capability/@name", Cap),
	Value = xml_factory:get_attribute("/capability/@value", Cap),
	Operator = list_to_atom(xml_factory:get_attribute("/capability/@operator", Cap)),
	{Name, {Value, Operator}}.
%% --------------------------------------------------------------------
%% Test functions
%% --------------------------------------------------------------------
get_key_test() ->
	Xml_Bin = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><query key=\"1111\">\t<capabilities>\t\t<capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"=\"/>\t\t<capability name=\"j2me_midp_1_1\" value=\"true\" operator=\"=\"/>\t</capabilities></query>">>,
	?assertEqual("1111", get_key(Xml_Bin)).
get_capabilities_test() ->
	Xml_Bin = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><query key=\"1111\">\t<capabilities>\t\t<capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"=\"/>\t\t<capability name=\"j2me_midp_1_1\" value=\"true\" operator=\"=\"/>\t</capabilities></query>">>,
	?assertEqual([{"j2me_cldc_1_1",{"true",'=='}},{"j2me_midp_1_1",{"true",'=='}}],  get_capabilities(Xml_Bin)).
get_timestamp_test() ->
	Xml_Bin = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><query><timestamp>19.12.2010</timestamp><capabilities>\t\t<capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"=\"/>\t\t<capability name=\"j2me_midp_1_1\" value=\"true\" operator=\"=\"/>\t</capabilities></query>">>,
	?assertEqual("19.12.2010",get_timestamp(Xml_Bin)).

get_timestamp_not_present_test() ->
	Xml_Bin = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><query><capabilities>\t\t<capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"=\"/>\t\t<capability name=\"j2me_midp_1_1\" value=\"true\" operator=\"=\"/>\t</capabilities></query>">>,
	?assertEqual([],get_timestamp(Xml_Bin)).
	

get_devices_test() ->
	Xml_Bin = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><query>\t<capabilities>\t\t<capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"=\"/>\t\t<capability name=\"j2me_midp_1_1\" value=\"true\" operator=\"=\"/>\t</capabilities></query>">>,
	D=get_devices(get_capabilities(Xml_Bin), "01.01.2010"),
	D1=xml_factory:to_xml(D),
	binary_to_list(D1),
%% 	xmerl_xpath:string("/device", D2),
 	?assertEqual(1841, erlang:length(D)).

xml_test() ->
	Xml_Bin = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><query>\t<capabilities>\t\t<capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"=\"/>\t\t<capability name=\"j2me_midp_1_1\" value=\"true\" operator=\"=\"/>\t</capabilities></query>">>,
	A=lists:flatten(xmerl:export_simple_content(get_devices(get_capabilities(Xml_Bin), "01.01.2010"), xmerl_xml)),
	?assertEqual(451787, erlang:length(A)).
