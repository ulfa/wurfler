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
-export([init/1, to_xml/2, post_is_create/2, content_types_provided/2, process_post/2]).
-compile([export_all]).
-include_lib("../deps/webmachine/include/webmachine.hrl").
-record(context, {devices=[]}).

%%
%% API Functions
%%
init([]) -> 
	{ok, #context{devices=[]}}.

content_types_provided(ReqData, Context) ->
    {[{"text/xml", to_xml}],ReqData, Context}.


allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

%% to_json(ReqData, Context) ->  
%% 	error_logger:info_msg("to_json ~n"),
%%     {"HBody", ReqData, Context}.


to_xml(ReqData, Context)->
	{ok, ReqData, Context}.

allow_missing_post(ReqData, Context) ->
	{true, ReqData, Context}.

post_is_create(ReqData, Context) ->
	{false, ReqData, Context}.

resource_exists(ReqData, Context) ->
	{true, ReqData, Context}.

process_post(ReqData, Context) ->
	Body = wrq:req_body(ReqData),
	Caps=get_capabilities(Body),
	D=lists:flatten(xmerl:export_simple_content(get_devices(Caps), xmerl_xml)),
	{true, wrq:append_to_response_body(D, ReqData), Context}.
%%
%% Local Functions
%%
get_capabilities(Body) ->
	Xml = xml_factory:parse(Body),
	Caps = xmerl_xpath:string ("/query/capabilities/capability", Xml),
	[create_cap(Cap)||Cap <- Caps].	

get_devices(Capabilities) ->
	wurfler:searchByCapabilities(Capabilities).

create_cap(Cap) ->
	Name = xml_factory:get_attribute("/capability/@name", Cap),
	Value = xml_factory:get_attribute("/capability/@value", Cap),
	Operator = list_to_atom(xml_factory:get_attribute("/capability/@operator", Cap)),
	{Name, {Value, Operator}}.
%% --------------------------------------------------------------------
%% Test functions
%% --------------------------------------------------------------------
get_capabilities_test() ->
	Xml_Bin = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><query>\t<capabilities>\t\t<capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"==\"/>\t\t<capability name=\"j2me_midp_1_1\" value=\"true\" operator=\"==\"/>\t</capabilities></query>">>,
	?assertEqual([{"j2me_cldc_1_1",{"true",'=='}},{"j2me_midp_1_1",{"true",'=='}}],  get_capabilities(Xml_Bin)).

get_devices_test() ->
	Xml_Bin = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><query>\t<capabilities>\t\t<capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"==\"/>\t\t<capability name=\"j2me_midp_1_1\" value=\"true\" operator=\"==\"/>\t</capabilities></query>">>,
	D=get_devices(get_capabilities(Xml_Bin)),
	io:format("1... ~p~n", [D]),
	D1=lists:flatten(xmerl:export_simple_content(D, xmerl_xml)),
	io:format("2... ~p~n", [D1]),
 	?assertEqual(1, erlang:length(D)).

xml_test() ->
	Xml_Bin = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><query>\t<capabilities>\t\t<capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"==\"/>\t\t<capability name=\"j2me_midp_1_1\" value=\"true\" operator=\"==\"/>\t</capabilities></query>">>,
	A=lists:flatten(xmerl:export_simple_content(get_devices(get_capabilities(Xml_Bin)), xmerl_xml)),
	?assertEqual(451787, erlang:length(A)).
