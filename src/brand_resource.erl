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
-include_lib("xmerl/include/xmerl.hrl").
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
    {['GET','DELETE', 'POST'], ReqData, Context}.

to_html(ReqData, #context{brand=Brand}=Context) ->
     {ok, Content} = brand_dtl:render(record_to_tuple(brand, Brand)),	 
     {Content, ReqData, Context}.

to_xml(ReqData, #context{brand = [Brand]} = Context) ->
    D = xml_factory:to_xml([xml_factory:create_xml(brand, Brand)]),
    {D, ReqData, Context}.

resource_exists(ReqData, Context) ->
	case get_brand(wrq:path_info(brand, ReqData)) of
		[] -> {false, ReqData, Context#context{brand=[]}};
		Brand -> {true, ReqData, Context#context{brand=Brand}}
	end.

delete_resource(ReqData, Context)->	
	delete_brand(wrq:path_info(brand, ReqData)),
	{true, ReqData, Context#context{brand=[]}}.

delete_completed(ReqData, Context) ->
	{true, ReqData, Context}.

post_is_create(ReqData, Context) ->
	{false, ReqData, Context}.

process_post(ReqData, Context) -> 
	ReqData1 = redirect("/brands", ReqData),
	delete_resource(ReqData1, Context).
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
redirect(Target, ReqData) ->
	Location = "http://" ++ wrq:get_req_header(?HOST, ReqData) ++ Target,
    Req=wrq:set_resp_header(?LOCATION, Location, ReqData),
	wrq:do_redirect(true, Req).
delete_brand(Brand_name) ->
	wurfler_db:delete_brand(Brand_name).
get_brand(Brand_Name) ->
	error_logger:info_msg("1... ~p~n", [Brand_Name]),
	wurfler:get_brand(mochiweb_util:unquote(Brand_Name)).
record_to_tuple(brand, [Brand]) ->
	[{brand, element(2, Brand)}, {models ,lists:keysort(2,element(3, Brand))}].
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
record_to_tuple_test() ->
	Brand = [{brand_index,"AI",[{"teleepoch_s570_ver1","S570"}]}] ,
	?assertEqual([{brand, "AI"},{models, [{"teleepoch_s570_ver1","S570"}]}], record_to_tuple(brand, Brand)).
