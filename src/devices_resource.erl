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
-export([init/1, to_json/2, to_xml/2, content_types_provided/2, resource_exists/2]).
-compile([export_all]).
-include_lib("../deps/webmachine/include/webmachine.hrl").
-record(context, {device}).

%%
%% API Functions
%%
init([]) -> 
	{ok, #context{device=undefined}}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json},{"text/plain",to_text}, {"text/xml", to_xml}],ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

to_json(ReqData, Context) ->  
	error_logger:info_msg("to_json ~n"),
    {"HBody", ReqData, Context}.

%% curl -d '@test/xml_caps_request.xml' -H "Accept: text/xml" -v http://localhost:8000/devices
to_xml(ReqData, #context{device=Device}=Context)->
	{ok, ReqData, Context}.

resource_exists(ReqData, Context) ->
	error_logger:info_msg("Capabilies : ~p~n", [wrq:req_body(ReqData)]),
	case get_device(wrq:req_body(ReqData)) of
		[] -> {false, ReqData, Context};
		Devices -> {true, ReqData, Context#context{device=Devices}}
	end.
%%
%% Local Functions
%%
get_device(Capabilities) ->
	wurfler:searchByCapabilities(Capabilities).

