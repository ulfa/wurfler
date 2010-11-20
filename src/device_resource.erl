%% Author: ua
%% Created: Nov 18, 2010
%% Description: TODO: Add description to device_resource
-module(device_resource).

%%
%% Include files
%%
-include("../include/wurfler.hrl").
-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-export([init/1, to_html/2, to_text/2, to_xml/2, content_types_provided/2, resource_exists/2]).

-include_lib("../deps/webmachine/include/webmachine.hrl").
-record(context, {device}).
%%
%% API Functions
%%
init([]) -> 
	{ok, #context{device=undefined}}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html},{"text/plain",to_text}, {"text/xml", to_xml}],ReqData, Context}.

to_text(ReqData, #context{device=Device}=Context) ->
	Body=Device#device.id,
    {Body, ReqData, Context}.

to_html(ReqData, Context) ->
    {Body, _RD, Ctx2} = to_text(ReqData, Context),
    HBody = io_lib:format("<html><body>~s</body></html>~n",[erlang:iolist_to_binary(Body)]),
    {HBody, ReqData, Ctx2}.

to_xml(ReqData, Context)->
	ok.
	
resource_exists(ReqData, Context) ->
	error_logger:info_msg("wrw:path() : ~p~n :" , [wrq:path(ReqData)]),
	error_logger:info_msg("wrw:path_info() : ~p~n :" , [wrq:path_info(ReqData)]),
	error_logger:info_msg("wrw:disp_path : ~p~n :" , [wrq:disp_path(ReqData)]),
	error_logger:info_msg("wrw:path_tokens() : ~p~n :" , [wrq:path_tokens(ReqData)]),
	error_logger:info_msg("wrw:get_qs_value() : ~p~n :" , [wrq:get_qs_value("ua",ReqData)]),
	case wrq:get_qs_value("useragent",ReqData) of
		[] -> {false, ReqData, Context}; 
		UserAgent -> case wurfler:searchByUA(UserAgent) of
						 [] -> {false, ReqData, Context};
						 Device -> {true, ReqData, Context#context{device=Device}}
					 end
	end.
%%
%% Local Functions
%%
get_device(useragent, Value) ->
	wurfler:searchByUA(Value);
get_device(devicename, Value) ->
	wurfler:searchByDeviceName(Value).
%%
%% Test Functions
%%
