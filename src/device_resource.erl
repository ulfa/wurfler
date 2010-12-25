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
-export([init/1, to_xml/2, to_html/2, content_types_provided/2, resource_exists/2]).
-compile([export_all]).
-include_lib("../deps/webmachine/include/webmachine.hrl").
-record(context, {device}).
%%
%% API Functions
%%
init([]) -> 
	{ok, #context{device=[]}}.

content_types_provided(ReqData, Context) ->
    {[{"text/xml", to_xml}, {"text/html", to_html}],ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

to_html(ReqData, #context{device=Device}=Context) ->
     {ok, Content} = device_dtl:render([{deviceDetail, record_to_tuple(device, Device)}]),
	 
     {Content, ReqData, Context}.

to_xml(ReqData, #context{device = Device} = Context) ->
    D = lists:flatten(xmerl:export_simple_content([xml_factory:create_xml(device, Device)], xmerl_xml)),
    {D, ReqData, Context}.

 
resource_exists(ReqData, Context) ->
	case get_device(wrq:path_info(device, ReqData), ReqData) of
		[] -> {false, ReqData, Context#context{device=[]}};
		Devices -> {true, ReqData, Context#context{device=Devices}}
	end.
%%
%% Local Functions
%%
get_device(undefined, ReqData) ->
error_logger:info_msg("UA ~p~n", [wrq:get_req_header("User-Agent", ReqData)]),
	wurfler:searchByUA(wrq:get_req_header("User-Agent", ReqData));
get_device(Value, _ReqData) ->
	wurfler:searchByDeviceName(Value).
%% --------------------------------------------------------------------
%%
%% --------------------------------------------------------------------
record_to_tuple(device, Record) ->
	Groups = record_to_tuple(groups, Record#device.groups, []),
	Keys = record_info(fields, device),
	Data = lists:nthtail(1,tuple_to_list(Record#device{groups=Groups})),
	lists:zip(Keys, Data);
record_to_tuple(group, Record) ->	
	Capabilities = record_to_tuple(capabilities, Record#group.capabilites, []),
	Keys = record_info(fields, group),
	Data = lists:nthtail(1,tuple_to_list(Record#group{capabilites=Capabilities})),
	lists:zip(Keys, Data);
record_to_tuple(capability, Record)->									  
	Keys = record_info(fields, capability),
	Data = lists:nthtail(1,tuple_to_list(Record)),
	lists:zip(Keys, Data).
record_to_tuple(groups, [], Acc) ->
	Acc;
record_to_tuple(groups, [Group|Groups], Acc) ->
	record_to_tuple(groups, Groups, lists:merge(record_to_tuple(group, Group), Acc));
record_to_tuple(capabilities, [], Acc) ->
	Acc;
record_to_tuple(capabilities, [Capability|Capabilities], Acc) ->
	record_to_tuple(capabilities, Capabilities, lists:merge(record_to_tuple(capability, Capability), Acc)).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
record_to_tuple_test() ->
	Device = #device{id="1", 
					 user_agent="ua", 
					 actual_device_root="true", 
					 fall_back="root", 
					 brand_name="Nokia", 
					 model_name="N95", 
					 groups=[#group{id="j2me", capabilites=[#capability{name="test", value="value"}, #capability{name="test_1", value="value_1"}]}],
					 created=undefined,
					 lastmodified=undefined},
	record_to_tuple(device,Device). 

	

	