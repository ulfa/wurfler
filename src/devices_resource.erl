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

to_text(ReqData, #context{device=Device}=Context) ->
	Body=Device#device.id,
    {Body, ReqData, Context}.

to_html(ReqData, Context) ->
    {Body, _RD, Ctx2} = to_text(ReqData, Context),
    HBody = io_lib:format("<html><body>~s</body></html>~n",[erlang:iolist_to_binary(Body)]),
    {HBody, ReqData, Ctx2}.

to_xml(ReqData, #context{device=Device}=Context)->
	{ok, ReqData, Context}.

resource_exists(ReqData, Context) ->
	true.


%%
%% Local Functions
%%

