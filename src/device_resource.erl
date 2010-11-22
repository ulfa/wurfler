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
-export([init/1, to_html/2, to_text/2, to_xml/2, content_types_provided/2, resource_exists/2]).
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

%% curl -H "Accept: text/plain" -v http://localhost:8000/device?useragent=rocker_ua
to_text(ReqData, #context{device=Device}=Context) ->
	Body=Device#device.id,
    {Body, ReqData, Context}.

%% curl -H "Accept: text/html" -v http://localhost:8000/device?useragent=rocker_ua
to_html(ReqData, Context) ->
    {Body, _RD, Ctx2} = to_text(ReqData, Context),
    HBody = io_lib:format("<html><body>~s</body></html>~n",[erlang:iolist_to_binary(Body)]),
    {HBody, ReqData, Ctx2}.

%% curl -H "Accept: text/xml" -v http://localhost:8000/device?useragent=rocker_ua
to_xml(ReqData, #context{device=Device}=Context)->
	D=lists:flatten(xmerl:export_simple_content([create_xml(device,Device)], xmerl_xml)),
	{D, ReqData, Context}.
	

%% curl -A "rocker_ua" -v http://localhost:8000/device 
resource_exists(ReqData, Context) ->
%% 	error_logger:info_msg("wrw:path() : ~p~n :" , [wrq:path(ReqData)]),
%% 	error_logger:info_msg("wrw:path_info() : ~p~n :" , [wrq:path_info(ReqData)]),
%% 	error_logger:info_msg("wrw:disp_path() : ~p~n :" , [wrq:disp_path(ReqData)]),
%% 	error_logger:info_msg("wrw:path_tokens() : ~p~n :" , [wrq:path_tokens(ReqData)]),
%% 	error_logger:info_msg("wrw:req_qs() : ~p~n :" , [wrq:req_qs(ReqData)]),	
%% 	error_logger:info_msg("wrw:get_qs_value() : ~p~n :" , [wrq:get_qs_value("device",ReqData)]),
%% 	error_logger:info_msg("wrw:req_headers() : ~p~n :" , [wrq:req_headers(ReqData)]),
%% 	error_logger:info_msg("wrw:path_info() : ~p~n :" , [wrq:path_info(device, ReqData)]),	
	case get_device(wrq:path_info(device, ReqData), ReqData) of
		[] -> {false, ReqData, Context};
		Device -> {true, ReqData, Context#context{device=Device}}
	end.

%%
%% Local Functions
%%
get_device(undefined, ReqData) ->
	wurfler:searchByUA(wrq:get_req_header("User-Agent", ReqData));
get_device(Value, _ReqData) ->
	wurfler:searchByDeviceName(Value).

%%
%% create the xml for devices
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


%%
%% Test helper functions
%%
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

parse(String) ->
	{Xml,_} = xmerl_scan:string(String),
	Xml.
get_attribute(XPath, Node) ->
	case xmerl_xpath:string(XPath, Node) of
		[#xmlAttribute{value = Value}] -> Value;
		O -> O
	end.
log_xml(Element)->
	B=lists:flatten(xmerl:export_simple_content(Element, xmerl_xml)),
	io:format("Xml: ~n~p~n", [B]).
	