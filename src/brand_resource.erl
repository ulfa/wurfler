%% Copyright 2010 Ulf
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

%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(brand_resource).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("../include/wurfler.hrl").

-export([init/1, to_xml/2, to_html/2, content_types_provided/2, resource_exists/2]).
-compile([export_all]).
-include_lib("../deps/webmachine/include/webmachine.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(context, {brand}).
%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
init([]) -> 
	{ok, #context{brand=[]}}.

content_types_provided(ReqData, Context) ->
    {[{"text/xml", to_xml}, {"text/html", to_html}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

to_html(ReqData, #context{brand=Brand}=Context) ->
     {ok, Content} = brand_dtl:render([{brand, Brand}]),
	 
     {Content, ReqData, Context}.

to_xml(ReqData, #context{brand = [Brand]} = Context) ->
    D = lists:flatten(xmerl:export_simple_content([xml_factory:create_xml(brand, Brand)], xmerl_xml)),
    {D, ReqData, Context}.

resource_exists(ReqData, Context) ->
	case get_brand(wrq:path_info(brand, ReqData)) of
		[] -> {false, ReqData, Context#context{brand=[]}};
		Brand -> {true, ReqData, Context#context{brand=Brand}}
	end.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
get_brand(Brand_Name) ->
	wurfler:get_brand(Brand_Name).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
