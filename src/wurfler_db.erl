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
-export([start/0,create_db/0, save_device/2, find_record_by_id/2, find_record_by_ua/2, get_first_device/1, get_next_device/2]).
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
	mnesia:create_table(devicesTbl,[{record_name, device},{disc_copies, [node()]}, {attributes, record_info(fields, device)}]),
	mnesia:create_table(j2meTbl,[{record_name, device},{disc_copies, [node()]}, {attributes, record_info(fields, device)}]),
	mnesia:create_table(symbianTbl,[{record_name, device},{disc_copies, [node()]}, {attributes, record_info(fields, device)}]),
	mnesia:create_table(blackberryTbl,[{record_name, device},{disc_copies, [node()]}, {attributes, record_info(fields, device)}]),
	%%mnesia:wait_for_tables([deviceTbl,j2meTbl,symbianTbl,blackberryTbl], 20000),
	application:stop(mnesia).

save_device(devicesTbl, Device)->
	mnesia:activity(transaction, fun() -> mnesia:write(devicesTbl, Device, write) end);
save_device(j2meTbl, Device)->
	mnesia:activity(transaction, fun() -> mnesia:write(j2meTbl, Device, write) end);
save_device(symbianTbl, Device)->
	mnesia:activity(transaction, fun() -> mnesia:write(symbianTbl, Device, write) end);
save_device(blackberryTbl, Device)->
	mnesia:activity(transaction, fun() -> mnesia:write(blackberryTbl, Device, write) end).

find_record_by_id(devicesTbl, Id) ->
	mnesia:activity(transaction, fun() -> mnesia:read({devicesTbl, Id}) end).
find_record_by_ua(devicesTbl, Ua) ->
	mnesia:activity(transaction, fun() -> qlc:e(qlc:q([P || P <- mnesia:table(devicesTbl), P#device.user_agent == Ua ])) end).
get_first_device(devicesTbl) ->
	mnesia:activity(transaction, fun() -> mnesia:first(devicesTbl) end).
get_next_device(devicesTbl, Key) ->
	mnesia:activity(transaction, fun() -> mnesia:next(devicesTbl, Key) end).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
find_record_by_id_test() ->
	?assertMatch([{device, "rocker", _, _,_,_}], find_record_by_id(devicesTbl, "rocker")).
find_record_by_ua_test() ->
	?assertMatch([{device, _,"rocker_ua", _,_,_}], find_record_by_ua(devicesTbl, "rocker_ua")).


