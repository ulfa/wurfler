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
-module(wurfler_db).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("../include/wurfler.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/0,create_db/0, save_device/2, find_record_by_id/2, find_record_by_ua/2, find_groups_by_id/2]).
-export([get_all_keys/1, find_capabilities_by_id/2]).
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
start() ->
	create_db().	
create_db() ->
	case mnesia:create_schema([node()]) of 
		{error, Reason} -> exit(Reason);
		_ -> error_logger:info_msg("schema created ~n")
	end,
	application:start(mnesia),
	mnesia:create_table(devicesTbl,[{type, set},{index, [user_agent,fall_back, actual_device_root]},{record_name, device},{disc_copies, [node()]}, {attributes, record_info(fields, device)}]),
	mnesia:create_table(j2meTbl,[{record_name, device},{disc_copies, [node()]}, {attributes, record_info(fields, device)}]),
	mnesia:create_table(symbianTbl,[{record_name, device},{disc_copies, [node()]}, {attributes, record_info(fields, device)}]),
	mnesia:create_table(blackberryTbl,[{record_name, device},{disc_copies, [node()]}, {attributes, record_info(fields, device)}]),
	mnesia:create_table(androidTbl,[{record_name, device},{disc_copies, [node()]}, {attributes, record_info(fields, device)}]),
	application:stop(mnesia).
%% --------------------------------------------------------------------
%% save functions
%% --------------------------------------------------------------------
save_device(devicesTbl, Device)->
	mnesia:activity(transaction, fun() -> mnesia:write(devicesTbl, Device, write) end);
save_device(j2meTbl, Device)->
	mnesia:activity(transaction, fun() -> mnesia:write(j2meTbl, Device, write) end);
save_device(symbianTbl, Device)->
	mnesia:activity(transaction, fun() -> mnesia:write(symbianTbl, Device, write) end);
save_device(blackberryTbl, Device)->
	mnesia:activity(transaction, fun() -> mnesia:write(blackberryTbl, Device, write) end).
%% --------------------------------------------------------------------
%% finder functions
%% --------------------------------------------------------------------
find_record_by_id(devicesTbl, Id) ->
	mnesia:dirty_read(devicesTbl, Id).
find_groups_by_id(devicesTbl, Id) ->
	[Device] = mnesia:dirty_read(devicesTbl, Id), {Device#device.fall_back, Device#device.groups}.
find_capabilities_by_id(devicesTbl, Id) ->
	[Device] = mnesia:dirty_read(devicesTbl, Id),
	Caps=lists:append(lists:foldl(fun(Group,Result) -> [Group#group.capabilites|Result] end, [], Device#device.groups)),
	{Device#device.fall_back, Caps}.
find_record_by_ua(devicesTbl, Ua) ->
	mnesia:activity(transaction, fun() -> qlc:e(qlc:q([P || P <- mnesia:table(devicesTbl), P#device.user_agent == Ua ])) end).
get_all_keys(devicesTbl) ->
	mnesia:dirty_select((devicesTbl),[{{device, '$1', '_', "true", '_', '_', '_', '_'}, [], ['$1']}]).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
find_record_by_id_test() ->
	?assertMatch([{device, "benq_s668c_ver1", _, _,_,_}], find_record_by_id(devicesTbl, "benq_s668c_ver1")).
find_record_by_ua_test() ->
	?assertMatch([{device, _,"Mozilla/4.1 (compatible; MSIE 5.0; Symbian OS; Nokia 7610", _,_,_}], find_record_by_ua(devicesTbl, "Mozilla/4.1 (compatible; MSIE 5.0; Symbian OS; Nokia 7610")).
find_group_by_id_test() ->
	find_groups_by_id(devicesTbl, "generic").
find_capabilities_by_id_test()->
	?assertMatch({"root", _}, find_capabilities_by_id(devicesTbl, "generic")).
get_all_keys_test() ->
	?assertEqual(6337,erlang:length(get_all_keys(devicesTbl))).