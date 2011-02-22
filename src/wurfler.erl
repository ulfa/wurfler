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
%%% Description : This module is responsible for searching the db .
%%% 
%%% Created : 
%%% -------------------------------------------------------------------
-module(wurfler).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/wurfler.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0]).
-export([searchByUA/1, searchByCapabilities/3, searchByDeviceName/1, getAllCapabilities/1, getVersion/0]).
-export([get_brands/0, get_brand/1, get_devices_by_model/1, check_device/3, delete_device/1, delete_brand/1]).
-compile([export_all]).
-define(TIMEOUT, infinity).
-define(CONTAINS, fun({device, [{model_name, Model_Name},_], []}) ->					   					   
					if Device#device.model_name == Model_Name -> true;
					    true -> false
					end
			   	  end).
%% ====================================================================
%% Record definition
%% ====================================================================
-record(state, {devices=[], groups=[], capabilities=[], generic=[]}).
%% ====================================================================
%% External functions
%% ====================================================================
import_wurfl(Filename) ->
	gen_server:cast(?MODULE, {import_wurfl, Filename}).
searchByCapabilities(Capabilities, Timestamp, Type) ->
	gen_server:call(?MODULE, {search_by_capabilities, Capabilities, Timestamp, Type}, ?TIMEOUT).
searchByUA(UserAgent)->
	gen_server:call(?MODULE, {search_by_ua, UserAgent}, ?TIMEOUT).
searchByDeviceName(DeviceName) ->
	gen_server:call(?MODULE, {search_by_device_id, DeviceName}, ?TIMEOUT).
getAllCapabilities(DeviceName)->
	gen_server:call(?MODULE, {get_all_capabilities, DeviceName}, ?TIMEOUT).
get_brands() ->
	gen_server:call(?MODULE, {get_brands}, ?TIMEOUT).
get_brand(Brand_Name) ->
	gen_server:call(?MODULE, {get_brand, Brand_Name}, ?TIMEOUT).
get_devices_by_model(Model_Name) ->
	gen_server:call(?MODULE, {get_devices_by_name, Model_Name}, ?TIMEOUT).
getVersion() ->
	gen_server:call(?MODULE, {version}).
check_device(Capabilities, Key, Id) ->
	gen_server:call(?MODULE, {check_device, Capabilities, Key, Id}, ?TIMEOUT).
%% @spec delete_device(Id::string()) -> List of device Ids
%% List = [string()] 
%% @doc This functions gets an device Id and than seeks all device which depends
%% on this device to delete them too.
delete_device(Id) ->
	gen_server:call(?MODULE, {delete_device, Id}, ?TIMEOUT).
delete_brand(Brand) ->
	gen_server:call(?MODULE, {delete_brand, Brand}, ?TIMEOUT).

%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
start() ->
	start_link().
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, new_state()}.
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({search_by_capabilities, Capabilities, Timestamp, Type}, _From, State) ->
	Result = search_by_capabilities(Capabilities, Timestamp, Type),
    {reply, Result, State};
handle_call({search_by_ua, User_Agent}, _From, State) ->
	Result = search_by_ua(User_Agent, State),
    {reply, Result, State};
handle_call({search_by_device_id, DeviceName}, _From, _State) ->
	Result=search_by_device_id(DeviceName),
    {reply, Result, new_state()};
handle_call({get_brands}, _From, State) ->
	{ok, Result}=get_all_brands(),
    {reply, Result, State};
handle_call({get_brand, Brand_Name}, _From, State) ->
	{ok, Result}=getBrand(Brand_Name),
    {reply, Result, State};	
handle_call({get_devices_by_name, Model_Name}, _From, _State) ->
	{ok, Result} = getDeviceByModelName(Model_Name),
	{reply, Result, new_state()};
handle_call({check_device, Capabilities, Key, Id}, _From, _State) ->
	Result = check_device(Capabilities, Key, Id, new_state()),
	{reply, Result, new_state()};
handle_call({delete_device, Id}, _From, State) ->
	Result = deleteDevice(Id),
	{reply, Result, State};
handle_call({delete_brand, Brand}, _From, State) ->
	Result = deleteBrand(Brand),
	{reply, Result, State};
handle_call({version}, _From, State) ->
    {reply, get_version(), State}.
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.
%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
get_version() ->
	{ok, Version} = application:get_key(wurflerservice, vsn),
	Version.
new_state() ->
	#state{devices=[], groups=[], capabilities=[]}.

search_by_device_id(DeviceName)->	
	case wurfler_db:find_record_by_id(devicesTbl, DeviceName) of
		[] -> [];
		[Device] -> Device				
	end.

search_by_ua(UserAgent, _State)->
	Device_Ids = wurfler_uaparser:parse(UserAgent),
	%%error_logger:info_msg("1... ~p~n", [Device_Ids]),
	Id = wurfler_search:searchByUA(UserAgent, Device_Ids),
	wurfler_search:search_by_device_id(Id).

search_by_capabilities(Capabilities, Timestamp, Type) ->
	wurfler_search:searchByCapabilities(Capabilities, Timestamp, Type).

check_device(Capabilities, Key, DeviceId, _State) ->
	wurfler_search:check_device(Capabilities, Key, DeviceId).

get_all_brands() ->
	{ok, wurfler_db:get_all_brands()}.
getBrand(Brand_Name) ->
	{ok, wurfler_db:get_brand(Brand_Name)}.
getDeviceByModelName(Model_Name) ->	
	R =  wurfler_db:get_devices_by_model_name(devicesTbl, Model_Name),
	Result = [xml_factory:create_device(D) || D <- R],
	{ok, Result}.

get_all_groups([], #state{groups=Groups}) ->
	{ok, #state{groups=Groups}};
get_all_groups("root", #state{groups=Groups}) ->
	{ok, #state{groups=Groups}};
get_all_groups("generic", #state{groups=Groups}) ->
 	{ok, #state{groups=Groups}};
get_all_groups(DeviceName, #state{groups=AllGroups}) ->
	{Fall_back, Groups} = wurfler_db:find_groups_by_id(devicesTbl, DeviceName),
	get_all_groups(Fall_back, #state{groups=lists:append(AllGroups,Groups)}).


%% @spec get_children_plus_parent(Id::string()) -> List
%%	List = [string()]
%% @doc This funktion finds all devices for given device, which depends
%% depends on this device. At least the function adds the device to the
%% list.
get_children_plus_parent(Id) ->
	case wurfler_db:find_id_by_fall_back(devicesTbl, Id) of
		[] -> [Id];
		List_of_ids -> flatten([Id|get_children(List_of_ids, [])])
	end.

get_children([], Acc) -> 
	Acc;
get_children([Fall_Back|Fall_Backs], Acc) ->
	Acc1 = [Fall_Back|Acc],
	case wurfler_db:find_id_by_fall_back(devicesTbl, Fall_Back) of
		[] -> get_children(Fall_Backs, Acc1);
		List -> L = lists:map(fun(I)->  wurfler:get_children_plus_parent(I) end, List),
				get_children(Fall_Backs, [L |Acc1])
	end.

flatten([]) ->
    [];
flatten([F|R]) when is_list(F) ->
    case io_lib:char_list(F) of
        true -> [F|flatten(R)];
        false -> flatten(F) ++ flatten(R)
    end;
flatten([F|R]) ->
    [F|flatten(R)].
%%------------------------------------------------------------------------------
%% Here i can optimize the create_fun stuff.
%% Perhaps i will use erl_scan and co.
%%------------------------------------------------------------------------------



add_device_to_devices(_Fun, Device, #state{devices=[]}=State) ->
	State#state{devices=[xml_factory:create_device(Device)|State#state.devices]};
add_device_to_devices(Fun, Device, #state{devices=Devices}=State) ->
 	case lists:any(Fun, Devices) of 
		false -> State#state{devices=[xml_factory:create_device(Device)|State#state.devices]};
		true -> State
	end.

deleteBrand(Brand_name) ->
	wurfler_db:delete_brand(Brand_name).

deleteDevice(Id) ->
	case get_children_plus_parent(Id) of
		[] -> [];
		List -> [wurfler_db:delete_device(Key) || Key <- List]
	end.
	
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
add_device_to_devices_test() ->
	Devices=[xml_factory:create_device(#device{id="1", brand_name="brand_1", model_name="model_1"}), 
			 xml_factory:create_device(#device{id="2", brand_name="brand_2", model_name="model_2"})],
	Device = #device{id="1", brand_name="brand_1", model_name="model_1"},
	State=add_device_to_devices(?CONTAINS, Device, #state{devices=Devices}),
	?assertEqual(2, erlang:length(State#state.devices)).
		
check_device() ->
	Caps=[{"handheldfriendly", {"true", '='}},
	 {"playback_mp4", {"false", '='}},
	 {"playback_wmv", {"none", '='}}],
	wurfler:check_device(Caps, [],"htc_desire_a8181_ver1").
	
wurfler_test_() ->
	{setup, 
	 	fun() -> setup() end,
	 	fun(_) ->
			[
			 ?_assertEqual(14,erlang:length(get_children_plus_parent("generic_android"))),		 
			 ?_assert([] =:= check_device()),
			 ?_assertEqual([{nok, "unknown"}], deleteDevice("unknown")), 
			 ?_assertEqual(14, erlang:length(deleteDevice("generic_android")))
			 ]
	 	end
	 }.

setup() ->
	mnesia:clear_table(devicesTbl),
	mnesia:clear_table(brand_index),
	mnesia:load_textfile("data/test.data").





	



