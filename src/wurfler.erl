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
-export([searchByUA/1, searchByCapabilities/2, searchByDeviceName/1, getAllCapabilities/1, getVersion/0]).
-export([get_brands/0, get_brand/1, get_devices_by_model/1]).
-compile([export_all]).
-define(TIMEOUT, infinity).
%% ====================================================================
%% Record definition
%% ====================================================================
-record(state, {devices=[], groups=[], capabilities=[], generic=[]}).
%% ====================================================================
%% External functions
%% ====================================================================
import_wurfl(Filename) ->
	gen_server:cast(?MODULE, {import_wurfl, Filename}).
searchByCapabilities(Capabilities, Timestamp) ->
	gen_server:call(?MODULE, {search_by_capabilities, Capabilities, Timestamp}, ?TIMEOUT).
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
handle_call({search_by_capabilities, Capabilities, Timestamp}, _From, State) ->
	Result=search_by_capabilities(Capabilities, Timestamp, new_state()),
    {reply, Result#state.devices, State};
handle_call({search_by_ua, User_Agent}, _From, State) ->
	Result = search_by_ua(User_Agent, State),
    {reply, Result, State};
handle_call({search_by_device_id, DeviceName}, _From, _State) ->
	Result=search_by_device_id(DeviceName),
    {reply, Result, new_state()};
handle_call({get_all_capabilities, DeviceName}, _From, _State) ->
	Generic = get_generic_capabilities(),
	{ok, Result}=get_all_capabilities(DeviceName, #state{capabilities=Generic}),
    {reply, Result#state.capabilities, new_state()};
handle_call({get_brands}, _From, State) ->
	{ok, Result}=get_all_brands(),
    {reply, Result, State};
handle_call({get_brand, Brand_Name}, _From, State) ->
	{ok, Result}=getBrand(Brand_Name),
    {reply, Result, State};	
handle_call({get_devices_by_name, Model_Name}, _From, _State) ->
	{ok, Result} = getDeviceByModelName(Model_Name),
	{reply, Result, new_state()};
handle_call({version}, _From, State) ->
    {reply, "0.1", State}.
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Request, State) ->
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
new_state() ->
	#state{devices=[], groups=[], capabilities=[]}.

search_by_device_id(DeviceName)->	
	case wurfler_db:find_record_by_id(devicesTbl, DeviceName) of
		[] -> [];
		[Device] -> Device				
	end.

search_by_ua(UserAgent, _State)->
	case wurfler_db:find_record_by_ua(devicesTbl, UserAgent) of
		[] -> [];
		[Device] -> {ok,#state{groups=Groups}} = get_all_groups(Device#device.id, new_state()),
					Device#device{groups=Groups}
	end.

search_by_capabilities(Capabilities, Timestamp, State) ->
	List_Of_Funs=create_funs_from_list(Capabilities),
	Keys = wurfler_db:get_all_keys(devicesTbl, Timestamp),
	get_devices_for_caps(List_Of_Funs, Keys, State#state{capabilities=get_generic_capabilities()}).

get_all_brands() ->
	{ok, wurfler_db:get_all_brands()}.
getBrand(Brand_Name) ->
	{ok, wurfler_db:get_brand(Brand_Name)}.
getDeviceByModelName(Model_Name) ->	
	R =  wurfler_db:get_devices_by_model_name(devicesTbl, Model_Name),
	Result = xml_factory:create_devices([xml_factory:create_device(D) || D <- R]),
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

get_all_capabilities([], #state{capabilities=Caps}) ->
	{ok, #state{capabilities=Caps}};
get_all_capabilities("root", #state{capabilities=Caps}) ->
	{ok, #state{capabilities=Caps}};
get_all_capabilities("generic", #state{capabilities=Caps}) ->
   	{ok, #state{capabilities=Caps}};
get_all_capabilities(DeviceName, #state{capabilities=Caps}) ->
	{Fall_back, Capabilities} = wurfler_db:find_capabilities_by_id(devicesTbl, DeviceName),	
	get_all_capabilities(Fall_back, #state{capabilities=overwrite(Caps, Capabilities)}).

get_generic_capabilities() ->
	{_Fall_back, Generic} = wurfler_db:find_capabilities_by_id(devicesTbl, "generic"),
	Generic.
%%------------------------------------------------------------------------------
%% Here i can optimize the create_fun stuff.
%% Perhaps i will use erl_scan and co.
%%------------------------------------------------------------------------------
create_funs_from_list(List) ->
	[create_fun(Name, Value, Operator) || {Name, {Value, Operator}} <- List].

create_fun(CheckName, CheckValue, '=')->
	fun(Name, Value) ->
		case Name of
			CheckName ->  case Value == CheckValue of
 							 true -> {ok};
							 false ->{nok}
						 end;
			_ -> {continue}
		end
	end;
create_fun(CheckName, CheckValue, '/=')->
	fun(Name, Value) ->			
		case Name of
			CheckName ->  case Value /= CheckValue of
							 true -> {ok};
							 false -> {nok}
						 end;
			_ -> {continue}
		end			
	end;
create_fun(CheckName, CheckValue, '>')->
	fun(Name, Value) ->
		case Name of
			CheckName ->  case Value > CheckValue of
							 true -> {ok};
							 false -> {nok}
						 end;
			_ -> {continue}
		end			
	end;
create_fun(CheckName, CheckValue, '<')->
	fun(Name, Value) ->
		case Name of
			CheckName ->  case Value < CheckValue of
							 true -> {ok};
							 false -> {nok}
						 end;
			_ -> {continue}
		end			
	end;
create_fun(CheckName, CheckValue, '=<')->
	fun(Name, Value) ->
		case Name of
			CheckName ->  case Value =< CheckValue of
							 true -> {ok};
							 false -> {nok}
						 end;
			_ -> {continue}
		end			
	end;
create_fun(CheckName, CheckValue, '>=')->
	fun(Name, Value) ->
		case Name of
			CheckName ->  case Value >= CheckValue of
							 true -> {ok};
							 false -> {nok}
						 end;
			_ -> {continue}
		end			
	end.

get_devices_for_caps([], _Keys, State) ->
	State#state{devices = xml_factory:create_devices(State#state.devices)};

get_devices_for_caps(_List_Of_Funs, [], State) ->
	State#state{devices = xml_factory:create_devices(State#state.devices)};

get_devices_for_caps(List_Of_Funs, [Key|Keys], State)->
	Device = search_by_device_id(Key),
	{ok, #state{capabilities=Caps}} = get_all_capabilities(Device#device.id, State),
	case run_funs_against_list(List_Of_Funs, Caps, {nok}) of
%%  	{ok} ->  get_devices_for_caps(List_Of_Funs, Keys, State#state{devices=[create_device(Device)|State#state.devices]});
  		{ok} ->  get_devices_for_caps(List_Of_Funs, Keys, add_device_to_devices(?CONTAINS, Device, State));
		{nok} -> get_devices_for_caps(List_Of_Funs, Keys, State)
	end.

add_device_to_devices(Fun, Device, #state{devices=Devices}=State) ->
 	case lists:any(Fun, Devices) of 
		false -> State#state{devices=[xml_factory:create_device(Device)|State#state.devices]};
		true -> State
	end.

run_funs_against_list(List_Of_Funs, [#capability{name=CheckName, value=CheckValue}|List_Of_Caps], Acc) ->
	case and_cond(List_Of_Funs, {CheckName, CheckValue}, []) of
		{ok} -> run_funs_against_list(List_Of_Funs, List_Of_Caps, {ok});
		{nok} -> run_funs_against_list(List_Of_Funs, [], {nok});
		[] -> run_funs_against_list(List_Of_Funs, List_Of_Caps, Acc)
	end;
run_funs_against_list(_List_Of_Funs, [], Acc) ->
	Acc.

and_cond([Fun|Funs], {CheckName, CheckValue}, Acc) ->
	case Fun(CheckName, CheckValue) of
		{ok} -> and_cond(Funs, {CheckName, CheckValue}, {ok});
		{nok} -> and_cond([], {CheckName, CheckValue}, {nok});
		_ -> and_cond(Funs, {CheckName, CheckValue}, Acc)
    end;
and_cond([], {_CheckName, _CheckValue}, Acc) -> 
	Acc.

overwrite(Generic, #capability{name=Name}=Capability) ->
	G=lists:keydelete(Name, 2,  Generic),
	[Capability|G];

overwrite(Generic, List_Of_Capabilities) ->
	overwrite(Generic, List_Of_Capabilities, Generic).
overwrite(_Generic, [], Acc) ->
	Acc;
overwrite(Generic, [Capability|List_Of_Capabilities], Acc) ->
	overwrite(Generic, List_Of_Capabilities, overwrite(Acc, Capability)).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
search_by_ua_test() ->
	search_by_ua("Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_1 like Mac OS X; de-de) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8B117 Safari/6531.22.7", new_state()).
add_device_to_devices_test() ->
	Devices=[xml_factory:create_device(#device{id="1", brand_name="brand_1", model_name="model_1"}), 
			 xml_factory:create_device(#device{id="2", brand_name="brand_2", model_name="model_2"})],
	Device = #device{id="1", brand_name="brand_1", model_name="model_1"},
	State=add_device_to_devices(?CONTAINS, Device, #state{devices=Devices}),
	?assertEqual(2, erlang:length(State#state.devices)).
	
	
and_cond_test() ->
	List=[{"device_os", {"iPhone OS", '='}}],
	List_Of_Funs = create_funs_from_list(List),
	?assertEqual({ok}, and_cond(List_Of_Funs, {"device_os","iPhone OS"}, [])),	
	?assertEqual({nok},and_cond(List_Of_Funs, {"device_os","unknown"}, [])),
	
	List1=[{"unknown", {"iPhone OS", '='}}],
	List_Of_Funs1 = create_funs_from_list(List1),
	?assertEqual([], and_cond(List_Of_Funs1, {"device_os","iPhone OS"}, [])).
	
create_function_test() ->
	A=create_fun("test", "123", '='),
	?assertEqual({ok}, A("test", "123")),
	?assertEqual({nok}, A("test", "12")),
	?assertEqual({continue}, A("unknown", "123")),

	A1=create_fun("device_os", "iPhone", '='),
	?assertEqual({ok}, A1("device_os", "iPhone")),
	?assertEqual({nok}, A1("device_os", "Android")),
	?assertEqual({continue}, A1("unknown", "123")),
	
	B=create_fun("test", "123", '/='),
	?assertEqual({ok}, B("test", "234")),
	?assertEqual({nok}, B("test" , "123")),
	?assertEqual({continue}, B("test1" , "234")),
	
	C=create_fun("test", 123, '>'),
	%% 234 > 123
	?assertEqual({ok}, C("test", 234)),
	?assertEqual({nok}, C("test", 23)),
	?assertEqual({continue}, C("unknown", 234)),
	
	D=create_fun("test", 123, '<'),
	?assertEqual({ok}, D("test", 24)),
	?assertEqual({nok}, D("test", 242)),
	?assertEqual({continue}, D("unknown", 234)),
	
	E=create_fun("test", 123, '=<'),
	%% 123 =< 123
	?assertEqual({ok}, E("test",123)),
	%% 12 =< 123
	?assertEqual({ok}, E("test",12)),
	%% 1234 =< 123
	?assertEqual({nok}, E("test",1234)),
	?assertEqual({continue}, E("unknow",123)),

	F=create_fun("test", 123, '>='),
	%% 123 >= 123
	?assertEqual({ok}, F("test",123)),
	%% 12 >= 123
	?assertEqual({nok}, F("test",12)),
	%% 1234 >= 123
	?assertEqual({ok}, F("test",1234)),
	?assertEqual({continue}, F("unknow",123)).

create_funs_from_list_test() ->
	List=[{"handheldfriendly", {"false", '='}},
	 {"playback_mp4", {"false", '='}},
	 {"playback_wmv", {"none", '='}}],
	?assertEqual(3, erlang:length(create_funs_from_list(List))).

run_funs_against_list_test()->
	List_of_para=[{"jpg", {"true", '='}},
	 {"gif", {"true", '='}},
	 {"png", {"true", '='}}],
	Caps1=wurfler:getAllCapabilities("generic_xhtml"),
	?assertEqual({ok},run_funs_against_list(create_funs_from_list(List_of_para), Caps1, [])),
		
 	List_of_para1=[{"handheldfriendly", {"false", '='}},
 	 {"playback_mp4", {"false", '='}},
 	 {"playback_wmv", {"none", '='}}],
 	?assertEqual({nok},run_funs_against_list(create_funs_from_list(List_of_para1), Caps1,{nok})),

	List_of_para2=[{"jpg", {"true", '='}},
	 {"gif", {"true", '='}},
	 {"png", {"false", '='}}],
	?assertEqual({nok},run_funs_against_list(create_funs_from_list(List_of_para2), Caps1, [])).

search_by_capabilities_test() ->
	List=[{"handheldfriendly", {"false", '='}},
	 {"playback_mp4", {"false", '='}},
	 {"playback_wmv", {"none", '='}}],
	State = search_by_capabilities(List, "01.01.2010", new_state()),
	io:format("~p~n", [State#state.devices]),
	?assertEqual(1, erlang:length(State#state.devices)).
	
search_by_capabilities_test_1() ->
	List=[{"device_os", {"iPhone OS", '='}}],
	List_Of_Funs=create_funs_from_list(List),
	Keys = ["apple_generic", "generic_xhtml"], 
	get_devices_for_caps(List_Of_Funs, Keys, new_state()).


overwrite_test() ->
	Generic = get_generic_capabilities(),
	C=[#capability{name="device_os_version", value="3.0"}, #capability{name="device_os", value="Test"}],
	?assertEqual(497,erlang:length(overwrite(Generic, C))).
	
	
	
	






	



