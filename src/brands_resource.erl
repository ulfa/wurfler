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
-module(brands_resource).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("../include/wurfler.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, to_xml/2, to_html/2, content_types_provided/2, resource_exists/2]).
-include_lib("../deps/webmachine/include/webmachine.hrl").
-compile([export_all]).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(context, {brands=[]}).
%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
init([]) -> 
	{ok, #context{brands=[]}}.

content_types_provided(ReqData, Context) ->
    {[{"text/xml", to_xml}, {"text/html", to_html}],ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

to_xml(ReqData, #context{brands=Brands}=Context) ->
	Brands_Xml = [xml_factory:create_xml(brand, Brand)|| Brand <- Brands],
	D = xml_factory:to_xml(Brands_Xml),
	{D, ReqData, Context}.

to_html(ReqData, #context{brands=Brands}=Context)->
	{ok, Content} = brands_dtl:render(record_to_tuple(brands, Brands)),
	{Content, ReqData, Context}.
 
resource_exists(ReqData, Context) ->
	case get_brands() of
		[] -> {false, ReqData, Context#context{brands=[]}};
		Brands -> {true, ReqData, Context#context{brands=Brands}}
	end.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
get_brands()->
	wurfler:get_brands().

record_to_tuple(brands, [], Acc) ->
	[{brands, lists:keysort(1,Acc)}];
record_to_tuple(brands, [Record|Records], Acc) ->
	record_to_tuple(brands, Records, [record_to_tuple(brand, Record) |Acc]).
record_to_tuple(brands, Records) ->
	record_to_tuple(brands, Records, []);
record_to_tuple(brand, Record) ->
	{element(2, Record), element(3, Record)}.

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
record_to_tuple_brand_test() ->
	Record={brand_index,"Nintendo", [{"nintendo_ds_ver1","DS"}, {"nintendo_wii_browser","Wii"}, {"nintendo_dsi_ver1","DSi"}]},
	?assertEqual({"Nintendo", [{"nintendo_ds_ver1","DS"}, {"nintendo_wii_browser","Wii"}, {"nintendo_dsi_ver1","DSi"}]}, record_to_tuple(brand, Record)).
record_to_tuple_brands_test() ->
	Records=[{brand_index,"Nintendo", [{"nintendo_ds_ver1","DS"}, {"nintendo_wii_browser","Wii"}, {"nintendo_dsi_ver1","DSi"}]},
			 {brand_index,"Nintendo1", [{"nintendo_ds_ver11","DS1"}, {"nintendo_wii_browser1","Wii1"}, {"nintendo_dsi_ver11","DSi1"}]}
			 ],
	?assertEqual(2, erlang:length(record_to_tuple(brands, Records))).
									