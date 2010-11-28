%% Author: ua
%% Created: Nov 18, 2010
%% Description: TODO: Add description to device_resource
-module(device_resource).

%%
%% Include files
%%
-include("../include/wurfler.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").
%%
%% Exported Functions
%%
-export([init/1, to_xml/2, content_types_provided/2, resource_exists/2]).
-compile([export_all]).
-include_lib("../deps/webmachine/include/webmachine.hrl").
-record(context, {device}).
%%
%% API Functions
%%
init([]) -> 
	{ok, #context{device=undefined}}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html},{"text/plain",to_text}, {"text/xml", to_xml}],ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

%% curl -H "Accept: text/plain" -v http://localhost:8000/device
%% to_text(ReqData, #context{device=Device}=Context) ->
%% 	Body=Device#device.id,
%%     {Body, ReqData, Context}.

%% curl -A "rocker_ua" -H "Accept: text/html" -v http://localhost:8000/device
%% to_html(ReqData, Context) ->
%%     {Body, _RD, Ctx2} = to_text(ReqData, Context),
%%     HBody = io_lib:format("<html><body>~s</body></html>~n",[erlang:iolist_to_binary(Body)]),
%%     {HBody, ReqData, Ctx2}.

%% curl -H "Accept: text/xml" -v http://localhost:8000/device/rocker
to_xml(ReqData, #context{device=Device}=Context)->
	D=lists:flatten(xmerl:export_simple_content([xml_factory:create_xml(device,Device)], xmerl_xml)),
	{D, ReqData, Context}.

%% curl -A "rocker_ua" -v http://localhost:8000/device 
resource_exists(ReqData, Context) ->
	case get_device(wrq:path_info(device, ReqData), ReqData) of
		[] -> {false, ReqData, Context};
		Device -> {true, ReqData, Context#context{device=Device}}
	end.

%%
%% Local Functions
%%
get_device(undefined, ReqData) ->
error_logger:info_msg("UA ~p~n", [wrq:get_req_header("User-Agent", ReqData)]),
	wurfler:searchByUA(wrq:get_req_header("User-Agent", ReqData));
get_device(Value, _ReqData) ->
	wurfler:searchByDeviceName(Value).