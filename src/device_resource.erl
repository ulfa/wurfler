%% Author: ua
%% Created: Nov 18, 2010
%% Description: TODO: Add description to device_resource
-module(device_resource).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-export([init/1, to_html/2, to_text/2, content_types_provided/2, resource_exists/2]).

-include_lib("../deps/webmachine/include/webmachine.hrl").
-record(context, {device}).
%%
%% API Functions
%%
init([]) -> 
	{ok, #context{device=undefined}}.

content_types_provided(ReqData, Context) ->
	error_logger:info_msg("in init/1 ~p~n",  [wrq:get_qs_value("useragent",ReqData) ]),
    {[{"text/html", to_html},{"text/plain",to_text}],ReqData, Context}.

to_text(ReqData, Context) ->
	Body=Context#context.device,
    {Body, ReqData, Context}.

to_html(ReqData, Context) ->
    {Body, _RD, Ctx2} = to_text(ReqData, Context),
    HBody = io_lib:format("<html><body>~s</body></html>~n",
                          [erlang:iolist_to_binary(Body)]),
    {HBody, ReqData, Ctx2}.

resource_exists(ReqData, Context) ->
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

%%
%% Test Functions
%%
