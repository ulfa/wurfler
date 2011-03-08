%% Author: Ulf Angermann
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
-export([init/1, to_xml/2, to_html/2, content_types_provided/2, resource_exists/2, 
		 delete_resource/2, delete_completed/2, allowed_methods/2, generate_etag/2]).
-export([post_is_create/2, process_post/2]).
-compile([export_all]).
-include_lib("../deps/webmachine/include/webmachine.hrl").
-record(context, {device, group}).
%%
%% API Functions
%%
init(_Config) -> 
 	{{trace, "/tmp"}, #context{device=[]}}.
	%%{ok, #context{device=[]}}.

content_types_provided(ReqData, Context) ->
    {[{"text/xml", to_xml}, {"text/html", to_html}],ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET', 'DELETE', 'POST'], ReqData, Context}.

to_html(ReqData, #context{device=Device}=Context) ->
     {ok, Content} = device_dtl:render(record_to_tuple(device, Device)),
     {Content, ReqData, Context}.

to_xml(ReqData, #context{device = Device} = Context) ->
    D = xml_factory:to_xml([xml_factory:create_xml(device, insertURI(ReqData, Device))]),
    {D, ReqData, Context}.
 
resource_exists(ReqData, Context) ->
	Device = wrq:path_info(device, ReqData),
	Group  = wrq:path_tokens(ReqData),
	process_request(Device, Group, ReqData, Context).
	
post_is_create(ReqData, Context) ->
	{false, ReqData, Context}.

process_post(ReqData, Context) ->
	ReqData1 = redirect("/brands", ReqData),
	delete_resource(ReqData1, Context).

delete_resource(ReqData, Context)->
	Device = wrq:path_info(device, ReqData),
	case delete_device(Device) of
		[] -> {false, ReqData, Context#context{device=[]}};
		_ -> {true, ReqData, Context#context{device=[]}}
	end.

delete_completed(ReqData, Context) ->
	{true, ReqData, Context}.
%%
%% Local Functions
%%
redirect(Target, ReqData) ->
	Location = "http://" ++ wrq:get_req_header(?HOST, ReqData) ++ Target,
    Req = wrq:set_resp_header(?LOCATION, Location, ReqData),
	wrq:do_redirect(true, Req).
	
delete_device(Id) ->
	wurfler:delete_device(Id).

process_request(undefined, [], ReqData, Context) ->
	error_logger:info_msg("UA ~p~n", [wrq:get_req_header("User-Agent", ReqData)]),
	case wurfler:searchByUA(wrq:get_req_header("User-Agent", ReqData)) of
		[] -> {false, ReqData, Context#context{device=[]}};
		Device -> {true, ReqData, Context#context{device=Device}}
	end;
process_request(Device_Id, [], ReqData, Context) ->
	get_device_by_id(ReqData, Context, Device_Id);

process_request(Device_Id, [Group_Name], ReqData, Context) ->
	get_device_by_id(ReqData, Context, Device_Id).

filter_group(Device, Group_Name) ->
	[Group || Group <- Device#device.groups].

insertURI(ReqData, Device) ->
	Device#device{id="http://" ++ wrq:get_req_header(?HOST, ReqData) ++ "/device/" ++ Device#device.id}.

%%expires(ReqData, Context) -> {wurfler_date_util:date_plus(calendar:now_to_datetime(now()), 1), ReqData, Context}.

generate_etag(ReqData, #context{device = Device} = Context) ->  {wurfler_util:generate_etag(Device), ReqData, Context}.

%% --------------------------------------------------------------------
%% internal functions
%% --------------------------------------------------------------------
get_device_by_id(ReqData, Context, Device_Id) ->
	
	case lookup_etag(wrq:get_req_header("If-None-Match", ReqData)) of
		[] ->  case wurfler:getDeviceById(Device_Id) of
					[] -> {false, ReqData, Context#context{device=[]}};
					Device -> wurfler_etag_cache:put(wurfler_util:generate_etag(Device), Device), 
							  {true, ReqData, Context#context{device=Device}}
				end;
		Device -> {true, ReqData, Context#context{device=Device}}
	end.

lookup_etag(undefined) ->
	[];
lookup_etag(Etag) ->
	wurfler_etag_cache:lookup(webmachine_util:split_quoted_strings(Etag)).
record_to_tuple(device, Record) ->
	Groups = lists:reverse(record_to_tuple(groups, Record#device.groups, [])),
	Keys = record_info(fields, device),
	Data = lists:nthtail(1,tuple_to_list(Record#device{groups=Groups})),
	[{picture, get_picture("priv/www/lib/devices/",Record#device.id)}|lists:zip(Keys, Data)];
record_to_tuple(group, Record) ->	
	Capabilities = record_to_tuple(capabilities, Record#group.capabilites, []),
	Data = lists:nthtail(1,tuple_to_list(Record#group{capabilites=Capabilities})),
	{lists:nth(1, Data), lists:nth(2, Data)};
record_to_tuple(capability, Record)->									  
	Keys = record_info(fields, capability),
	Data = lists:nthtail(1,tuple_to_list(Record)),
	lists:zip(Keys, Data).
record_to_tuple(groups, [], Acc) ->
	Acc;
record_to_tuple(groups, [Group|Groups], Acc) ->
	record_to_tuple(groups, Groups, [record_to_tuple(group, Group)| Acc]);
record_to_tuple(capabilities, [], Acc) ->
	Acc;
record_to_tuple(capabilities, [Capability|Capabilities], Acc) ->
	record_to_tuple(capabilities, Capabilities, lists:merge(record_to_tuple(capability, Capability), Acc)).

get_picture(Path, Id) ->
	case filelib:is_regular(Path ++ Id ++ ".gif") of
		true -> Id ++ ".gif";
		false -> "no_image.gif"
	end.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
get_picture_test() ->
	{ok, Path} = file:get_cwd(),
	case string:rstr(Path, ".eunit") > 0 of
		true -> file:set_cwd("..");
		false -> false
	end,
	{ok, Pwd} = file:get_cwd(),
	?assertEqual("acer_e101_ver1.gif", get_picture(lists:append(Pwd, "/priv/www/lib/devices/"), "acer_e101_ver1")).
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
	io:format("2... : ~p~n" ,[record_to_tuple(device,Device)]). 

	
device_resource_test_() ->
	{setup, 
	 	fun() -> setup() end,
	 	fun(_) -> teardown() end,
	 	fun(_) ->
			[
			 ?_assertEqual([{nok, "unknown"}],delete_device("unknown")),
			 ?_assertEqual(14,erlang:length(delete_device("generic_android")))		 
			 ]
	 	end
	 }.
setup() ->
	wurfler_test_setup:setup(),
	mnesia:load_textfile("data/test.data").
teardown() ->
	wurfler_test_setup:teardown().
