%% Author: ua
%% Created: Nov 18, 2010
%% Description: TODO: Add description to device_resource

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
    {[{"text/xml", to_xml}],ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.


%% to_text(ReqData, #context{device=Device}=Context) ->
%% 	Body=Device#device.id,
%%     {Body, ReqData, Context}.


%% to_html(ReqData, Context) ->
%%     {Body, _RD, Ctx2} = to_text(ReqData, Context),
%%     HBody = io_lib:format("<html><body>~s</body></html>~n",[erlang:iolist_to_binary(Body)]),
%%     {HBody, ReqData, Ctx2}.


to_xml(ReqData, #context{device=Device}=Context)->
	D=lists:flatten(xmerl:export_simple_content([xml_factory:create_xml(device,Device)], xmerl_xml)),
	{D, ReqData, Context}.

 
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