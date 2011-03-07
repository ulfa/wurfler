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
%%%
%%% Description : This module is responsible for reading and storing
%%% devices from the database.
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
-export([find_capabilities_by_id/2,get_all_keys/1,get_all_keys/2, save_brand_index/2, get_all_brands/0]).
-export([get_brand/1, get_devices_by_model_name/2, save_capabilities_devices/1, get_capablities_devices/1]).
-export([clear_capabilities_devices/0, find_devices_by_brand/2, find_capabilities_device_by_key/1]).
-export([delete_device/1, delete_brand/1, save_changed_caps_devices/1, find_changed_caps_devices/1]).
-export([get_all_cap_key/1, find_id_by_fall_back/2, find_os_device_id/1, save_os_device_id/1]).
-export([find_group_of_device/3, get_keys/1, get_all_group_names/0]).
-export([lookup/1, delete/1, clear/0, put/2]).
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
	mnesia:create_table(devicesTbl,[{type, set},{index, [user_agent,fall_back, actual_device_root]},
									{record_name, device},{disc_copies, [node()]}, {attributes, record_info(fields, device)}]),
	mnesia:create_table(brand_index,[{disc_copies, [node()]}, {attributes, record_info(fields, brand_index)}]),
	mnesia:create_table(changed_caps_devices,[{record_name, capabilities_devices}, {disc_copies, [node()]}, {attributes, record_info(fields, capabilities_devices)}]),
	mnesia:create_table(capabilities_devices,[{disc_copies, [node()]}, {attributes, record_info(fields, capabilities_devices)}]),
	mnesia:create_table(capability_description,[{disc_copies, [node()]}, {attributes, record_info(fields, capability_description)}]),
	mnesia:create_table(os_device_id, [{disc_copies, [node()]}, {attributes, record_info(fields, os_device_id)}]),
	mnesia:create_table(etag_cache, [{ram_copies, [node()]}, {attributes, record_info(fields, etag_cache)}]),
	mnesia:wait_for_tables([devicesTbl, brand_index, capabilities_devices, changed_caps_devices, capability_description, os_device_id], 100000),
	upload_os_device_id(),
	application:stop(mnesia).
%% --------------------------------------------------------------------
%% save functions
%% --------------------------------------------------------------------
save_device(devicesTbl, Device)->
	mnesia:activity(transaction, fun() -> mnesia:write(devicesTbl, Device, write) end).

save_brand_index([], {Id, Model_Name}) ->
	error_logger:error_msg("can't save the data, because of a missing brand_name for model : ~p and Id : ~p~n", [Model_Name, Id]);
save_brand_index(Brand_Name, {Id, Model_Name}) ->
	mnesia:activity(transaction, fun() ->
		case mnesia:read(brand_index, Brand_Name) of
			[] -> mnesia:write(brand_index, #brand_index{brand_name=Brand_Name, models=[{Id, Model_Name}]}, write);
			[Record] -> mnesia:write(brand_index, Record#brand_index{models=[{Id, Model_Name}|Record#brand_index.models]}, write)
		end
	end).
save_brand_index(Brand_index) ->
	mnesia:activity(transaction, fun() -> mnesia:write(brand_index, Brand_index, write) end).
save_capabilities_devices(Caps_Devices) ->
	mnesia:activity(transaction, fun() -> mnesia:write(capabilities_devices, Caps_Devices, write) end).
save_changed_caps_devices(Caps_Devices) ->
	mnesia:activity(transaction, fun() -> mnesia:write(changed_caps_devices, Caps_Devices, write) end).
save_os_device_id(Os_Device_Id) ->
	mnesia:activity(transaction, fun() -> mnesia:write(os_device_id, Os_Device_Id, write) end).
%% --------------------------------------------------------------------
%% finder functions
%% --------------------------------------------------------------------
find_record_by_id(devicesTbl, Id) ->
	mnesia:dirty_read(devicesTbl, Id).

find_groups_by_id(devicesTbl, Id) ->
	[Device] = find_record_by_id(devicesTbl, Id), 
	{Device#device.fall_back, Device#device.groups}.

find_group_of_device(devicesTbl, Device_Id, Group_Name) ->
	[Device] = find_record_by_id(devicesTbl, Device_Id), 
	[Group || Group <- Device#device.groups, Group#group.id =:= Group_Name].	

find_capabilities_by_id(devicesTbl, Id) ->
	case find_record_by_id(devicesTbl, Id) of
		[Device] -> Caps = lists:append(lists:foldl(fun(Group,Result) -> [Group#group.capabilites|Result] end, [], Device#device.groups)),
					{Device#device.fall_back, Caps};
		[] -> {[], []}
	end.

find_record_by_ua(devicesTbl, Ua) ->
	mnesia:activity(sync_dirty, fun() -> qlc:e(qlc:q([P || P <- mnesia:table(devicesTbl), P#device.user_agent == Ua])) end).
find_id_by_fall_back(devicesTbl, Fall_Back) ->
	mnesia:activity(sync_dirty, fun() -> qlc:e(qlc:q([P#device.id || P <- mnesia:table(devicesTbl), P#device.fall_back == Fall_Back])) end).
get_all_cap_key(capabilities_devices) ->
	mnesia:activity(sync_dirty, fun() -> qlc:e(qlc:q([{P#capabilities_devices.capabilities, P#capabilities_devices.key} || P <- mnesia:table(capabilities_devices)])) end).
get_keys(devicesTbl) ->
	mnesia:dirty_all_keys(devicesTbl).
get_all_keys(devicesTbl) ->
	get_all_keys(devicesTbl, ?DEFAULT_TIMESTAMP);
get_all_keys(os_device_id) ->
	mnesia:dirty_all_keys(os_device_id).
get_all_keys(devicesTbl, ?DEFAULT_TIMESTAMP) ->
 	mnesia:activity(sync_dirty, fun() -> qlc:e(qlc:q([P#device.id || P <- mnesia:table(devicesTbl), P#device.actual_device_root =:= "true" ])) end);
get_all_keys(devicesTbl, Timestamp) ->
	T = wurfler_date_util:parse_to_datetime(Timestamp),
	mnesia:activity(sync_dirty, fun() -> 
								qlc:e(qlc:q([P#device.id || P <- mnesia:table(devicesTbl), P#device.lastmodified > T,  P#device.actual_device_root =:= "true"])) 
								end).
get_all_brands()->
	mnesia:activity(sync_dirty, fun() -> 
								qlc:e(qlc:q([P || P <- mnesia:table(brand_index)]))
								end).
get_brand(Brand_Name) ->
	mnesia:dirty_read(brand_index, Brand_Name).
find_devices_by_brand(devicesTbl, Brand_name) ->
	mnesia:activity(sync_dirty, fun() -> qlc:e(qlc:q([P || P <- mnesia:table(devicesTbl), P#device.brand_name == Brand_name])) end).
get_devices_by_model_name(devicesTbl, Model_Name) ->
		mnesia:activity(sync_dirty, fun() -> qlc:e(qlc:q([P || P <- mnesia:table(devicesTbl), P#device.model_name == Model_Name])) end).
get_capablities_devices(Capapbilities) ->
	mnesia:dirty_read(Capapbilities).
find_capabilities_device_by_key(Key) ->
	mnesia:activity(sync_dirty, fun() -> qlc:e(qlc:q([P || P <- mnesia:table(capabilities_devices), P#capabilities_devices.key =:= Key])) end).
find_changed_caps_devices(Timestamp) ->
	T = wurfler_date_util:parse_to_datetime(without_dot,Timestamp),
	mnesia:activity(sync_dirty, fun() -> 
								qlc:e(qlc:q([P || P <- mnesia:table(changed_caps_devices), P#capabilities_devices.lastmodified > T])) 
								end).
find_os_device_id(OS) ->
	mnesia:dirty_read(os_device_id, OS).
get_all_group_names() ->
	[Generic] = find_record_by_id(devicesTbl, "generic"),
	[Group#group.id || Group <- Generic#device.groups].
%% --------------------------------------------------------------------
%%% Other functions
%% --------------------------------------------------------------------
clear_capabilities_devices() ->
	mnesia:activity(transaction, fun() -> mnesia:clear_table(capabilities_devices) end).
upload_os_device_id() ->
	error_logger:info_msg("start : load the os_device_id content~n"),
	mnesia:load_textfile("data/tables.dump"),
	error_logger:info_msg("end : load the os_device_id content~n").
%% --------------------------------------------------------------------
%%% delete functions
%% --------------------------------------------------------------------
delete_device("generic") ->
	error_logger:info_msg("can't delete the generic device"),
	[];
delete_device(Id) ->
	mnesia:activity(transaction, fun() ->
		case find_record_by_id(devicesTbl, Id) of
			[] -> {nok, Id};
			[Device] -> remove_device_from_brand(Device),
						mnesia:delete(devicesTbl, Device#device.id, write),
						error_logger:info_msg("deleted device with id : ~p~n", [Device#device.id]),
						{ok, Device#device.id}
		end
	end).

delete_brand(Brand_name) ->
	mnesia:activity(transaction, fun() ->
		case find_devices_by_brand(devicesTbl, Brand_name) of
			[] -> [];
			Devices -> [mnesia:delete(devicesTbl, Device#device.id, write)|| Device <- Devices]
		end,
		 mnesia:delete(brand_index, Brand_name, write)
	end).
	
remove_device_from_brand(#device{id=Id, brand_name=Brand_name}) ->
	case get_brand(Brand_name) of
		[] -> [];
		[Brand_index] -> Models = lists:keydelete(Id, 1, Brand_index#brand_index.models),
						case Models of
							[] -> mnesia:delete(brand_index, Brand_name, write); 
							_ -> save_brand_index(Brand_index#brand_index{models=Models})
						end
	end.

%% --------------------------------------------------------------------
%%% etag cache functions
%% --------------------------------------------------------------------
lookup(Key) ->
	mnesia:dirty_read(etag_cache, Key).
clear() ->
	mnesia:clear_table(etag_cache).
put(Key, Term) ->
	mnesia:write(etag_cache, #etag_cache{id=Key, term=Term}).
delete(Key) ->
	mnesia:delete(etag_cache, Key).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
find_group_of_device_test() ->
	?assertMatch([{group,"sound_format",_}],find_group_of_device(devicesTbl, "generic","sound_format")).

wurfler_db_test_() ->
	{setup, 
	 	fun() -> setup() end,
	 	fun() -> teardown() end,
	 	fun() ->
			[
			 ?_assertEqual(2,erlang:length(get_all_keys(devicesTbl, "01.01.2010"))),
			 ?_assertEqual(0,erlang:length(get_all_keys(devicesTbl, "01.01.2099"))),
			 ?_assertMatch({"root", _}, find_capabilities_by_id(devicesTbl, "generic")),
			 ?_assertMatch([{device, "htc_desire_a8181_ver1_sub2_2",_,_,_,_,_,_,_,_,_}], find_record_by_id(devicesTbl, "htc_desire_a8181_ver1_sub2_2")),
			 ?_assertMatch([{device, _Id,"Mozilla/5.0 (Linux; U; Android 2.1-update1; en-au; GT-I9000T Build/ECLAIR) AppleWebKit/530.17 (KHTML, like Gecko) Version/4.0 Mobile Safari/530.17", _,_,_,_,_,_,_,_}], 
				 find_record_by_ua(devicesTbl, "Mozilla/5.0 (Linux; U; Android 2.1-update1; en-au; GT-I9000T Build/ECLAIR) AppleWebKit/530.17 (KHTML, like Gecko) Version/4.0 Mobile Safari/530.17")),
			 ?_assertEqual({ok,"samsung_gt_i9000_ver1"},delete_device("samsung_gt_i9000_ver1")),
			 ?_assert({nok, "not_known"} =:= delete_device("not_known")),
			 ?_assertEqual(2, erlang:length(find_id_by_fall_back(devicesTbl, "generic"))),
			 ?_assertEqual(30,erlang:length(get_all_group_names()))
			 ]
	 	end
	 }.
	
setup() ->
	wurfler_test_setup:setup(),
	mnesia:load_textfile("data/test.data").
teardown() ->
	wurfler_test_setup:teardown().
	
    