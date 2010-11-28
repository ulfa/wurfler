%% Author: ulfangermann
%% Created: Nov 23, 2010
%% Description: TODO: Add description to devices_resource
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
-export([init/1, to_json/2, to_xml/2, post_is_create/2, content_types_provided/2, process_post/2]).
-compile([export_all]).
-include_lib("../deps/webmachine/include/webmachine.hrl").
-record(context, {devices=[]}).

%%
%% API Functions
%%
init([]) -> 
	{ok, #context{devices=[]}}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}, {"text/xml", to_xml}],ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

to_json(ReqData, Context) ->  
	error_logger:info_msg("to_json ~n"),
    {"HBody", ReqData, Context}.

%% curl -d '@test/xml_caps_request.xml' -H "Accept: text/xml" -v http://localhost:8000/devices
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

body_test() ->
	Xml_Bin = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><query>\t<capabilities>\t\t<capability name=\"j2me_cldc_1_1\" value=\"true\" operator=\"==\"/>\t\t<capability name=\"j2me_midp_1_\" value=\"true\" operator=\"==\"/>\t</capabilities></query>">>,
	Xml = xml_factory:parse(Xml_Bin),
	Caps = xmerl_xpath:string ("/query/capabilities/capability", Xml),
	[create_cap(Cap)||Cap <- Caps].
	

