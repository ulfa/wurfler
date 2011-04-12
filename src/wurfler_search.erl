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
-module(wurfler_search).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("../include/wurfler.hrl").
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-compile([export_all]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0]).
-export([searchByUA/2, searchByCapabilities/3, check_device/3, searchByDeviceId/1]).
-export([getAllCapabilities/1, get_capability/2, get_all_groups/1]).
-define(TIMEOUT, infinity).

%% ====================================================================
%% External functions
%% ====================================================================
searchByCapabilities(Capabilities, Timestamp, Type) ->
	gen_server:call(?MODULE, {search_by_capabilities, Capabilities, Timestamp, Type}, ?TIMEOUT).
searchByUA(UserAgent, Device_Ids)->
	gen_server:call(?MODULE, {search_by_ua, UserAgent, Device_Ids}, ?TIMEOUT).
searchByDeviceId(Id)->
	gen_server:call(?MODULE, {search_by_device_id, Id}, ?TIMEOUT).
check_device(Capabilities, Key, Id) ->
	gen_server:call(?MODULE, {check_device, Capabilities, Key, Id}, ?TIMEOUT).
getAllCapabilities(Device_Id) ->
	gen_server:call(?MODULE, {get_all_capabilities, Device_Id}).

%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {devices=[], groups=[], capabilities=[], generic=[]}).
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
    {ok, #state{devices=[], groups=[], capabilities=[]}}.

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
	Result = search_by_capabilities(Capabilities, Timestamp, Type, new_state()),
    {reply, Result, State};
handle_call({search_by_ua, User_Agent, Device_Ids}, _From, State) ->
	Result = search_by_ua(User_Agent, Device_Ids, State),
    {reply, Result, State};
handle_call({search_by_device_id, Id}, _From, State) ->
	Result = search_by_device_id(Id),
    {reply, Result, State};
handle_call({check_device, Capabilities, Key, Id}, _From, _State) ->
	Result = check_device(Capabilities, Key, Id, new_state()),
	{reply, Result, new_state()};
handle_call({get_all_capabilities, Device_Id}, _From, State) ->
	Result = get_all_capabilities(wurfler_db:find_list_of_devices_with_generic(devicesTbl, Device_Id)),
    {reply, Result, State}.

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
new_state() ->
	#state{devices=[], groups=[], capabilities=[]}.

search_by_capabilities(Capabilities, Timestamp, [], _State) ->
	Keys = get_keys([], Timestamp),
	search_by_capabilities(Capabilities, Keys);
search_by_capabilities(Capabilities, Timestamp, Type, _State) ->
	Keys = get_keys(Type, Timestamp),
	search_by_capabilities(Capabilities, Keys).

search_by_capabilities(Capabilities, Keys) ->
	Caps = extract_only_need_capabilities(get_generic_capabilities(), Capabilities),
	List_Of_Funs = create_funs_from_list(Capabilities),
	pmap(List_Of_Funs, Caps, wurfler_util:split_list(Keys, erlang:system_info(schedulers))).

get_keys([], Timestamp) ->
	wurfler_db:get_all_keys(devicesTbl, Timestamp);
get_keys(Type, _Timestamp) ->
	case wurfler_db:find_os_device_id(Type) of
		[#os_device_id{device_ids=Device_Ids}] -> Device_Ids;
		[] -> []
	end.

pmap(_List_Of_Funs, _Capabilities, []) ->
	xml_factory:create_devices([]);
pmap(List_Of_Funs, Capabilities, Keys) ->
	Parent = self(),
	Pids = lists:map(fun(Key) -> 
						proc_lib:spawn_link(fun() -> do_it(Parent, List_Of_Funs, Capabilities, Key) end) 
					 end, Keys),
	xml_factory:create_devices(gather(Pids, [])).

do_it(Parent, List_Of_Funs, Capabilities, Keys) ->
	Parent ! {get_devices_for_caps(List_Of_Funs, Keys, #state{capabilities=Capabilities})}.

gather([_Pid|Pids], Acc) ->
	receive
		{[]} -> gather(Pids, Acc);
		{Devices} -> gather(Pids, lists:append(Devices,Acc))
	end;
gather([], Acc) ->
	Acc.

search_by_device_id_only(Device_Id)->	
	case wurfler_db:find_record_by_id(devicesTbl, Device_Id) of
		[] -> [];
		[Device] -> Device				
	end.

search_by_device_id(Device_Id)->	
	case wurfler_db:find_record_by_id(devicesTbl, Device_Id) of
		[] -> [];
		[Device] -> Device#device{groups=get_all_groups(Device_Id)}				
	end.

search_by_ua(UserAgent, Device_Ids, _State)->
	case wurfler_string_metrics:levenshtein(useragent, Device_Ids, UserAgent) of
		{Distance, Id, Ua} ->  error_logger:info_msg("Distance : ~p Prozent : ~p~n", [Distance, calculate(Distance, UserAgent, Ua)]),
							   case calculate(Distance, UserAgent, Ua) > 20 of 
									true -> [];
									false -> Id
								end;
		[] -> []
	end.

calculate(Distance, UA_1, UA_2) ->
	Distance * 100 / (erlang:length(UA_1) + erlang:length(UA_2)).

check_device(Capabilities, Key, DeviceId, State) ->
	List_Of_Funs = create_funs_from_list(Capabilities),
	State1 = get_devices_for_caps(List_Of_Funs, [DeviceId], State#state{capabilities=extract_only_need_capabilities(get_generic_capabilities(), Capabilities)}),
	case State1#state.devices of
		[] -> [];
		[{devices,[],[]}] -> [];
		Devices -> {Capabilities, Key, Devices}
	end.

get_all_groups(Device_Id) ->
	List_of_devices = wurfler_db:find_list_of_devices_with_generic(devicesTbl, Device_Id),
	get_all_groups(List_of_devices,  #state{groups=[]}).
get_all_groups([], #state{groups=Groups}) ->
	lists:keysort(2, Groups);
get_all_groups("root",  #state{groups=Groups}) ->
	lists:keysort(2, Groups);
get_all_groups([Device_Id|Device_Ids], #state{groups=AllGroups}) ->
	{_Fall_back, Groups} = wurfler_db:find_groups_by_id(devicesTbl, Device_Id),
	get_all_groups(Device_Ids, #state{groups=check_groups(AllGroups, Groups)}).

check_groups(AllGroups, []) ->
	AllGroups;
check_groups(AllGroups, [Group|Groups]) ->
	AllGroups1 = case lists:keyfind(Group#group.id, 2, AllGroups) of 
					false -> [Group|AllGroups];
					Group_Of_All -> Caps = check_capabilities(Group_Of_All#group.capabilites, Group#group.capabilites),
									lists:keyreplace(Group#group.id, 2, AllGroups, Group#group{capabilites=Caps})
				 end,		
	check_groups(AllGroups1, Groups).

check_capabilities(AllCapabilities, []) ->
	AllCapabilities;
check_capabilities(AllCapabilities, [Capability|Capabilities]) ->
	AllCapabilities1 = case lists:keyfind(Capability#capability.name, 2, AllCapabilities) of
						   	false -> error_logger:error_info("The capability ~p is not in the pool of capabilities", [Capability]),
								    AllCapabilities;
						   _ -> lists:keyreplace(Capability#capability.name, 2, AllCapabilities, Capability)
					   end,
	check_capabilities(AllCapabilities1, Capabilities).


get_all_capabilities(List_Of_Fall_Back) ->
	get_all_capabilities(List_Of_Fall_Back, []).

get_all_capabilities([], Acc) ->
	Acc;
get_all_capabilities([Fall_Back|List_Of_Fall_Back], Acc) ->
	Capabilities = wurfler_db:find_capabilities_by_id (devicesTbl, Fall_Back),
	get_all_capabilities(List_Of_Fall_Back, overwrite(capabilities, Capabilities, Acc)).

get_generic_capabilities() ->
	wurfler_db:find_capabilities_by_id(devicesTbl, "generic").

add_device_to_devices(Device, State) ->
	State#state{devices=[xml_factory:create_device(Device)|State#state.devices]}.

get_devices_for_caps([], _Keys, State) ->
	State#state.devices;
get_devices_for_caps(_List_Of_Funs, [], State) ->
	State#state.devices;
get_devices_for_caps(List_Of_Funs, [Key|Keys], #state{capabilities=Needed_Caps}=State)->
	Caps = get_all_capabilities(wurfler_db:find_list_of_devices(devicesTbl, Key), Needed_Caps),
	case run_funs_against_list(List_Of_Funs, Caps, {nok}) of
  		{ok} ->  get_devices_for_caps(List_Of_Funs, Keys, add_device_to_devices(search_by_device_id_only(Key), State));
		{nok} -> get_devices_for_caps(List_Of_Funs, Keys, State)
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
  

overwrite(capability, #capability{name = Name} = Capability, Acc) ->
	lists:keyreplace(Name, 2, Acc, Capability);

overwrite(capabilities, List_Of_Capabilities, []) ->
	overwrite(capabilities, List_Of_Capabilities, List_Of_Capabilities);
overwrite(capabilities, [], Acc) ->
	Acc;
overwrite(capabilities, [Capability|List_Of_Capabilities], Acc) ->
	Acc1 = overwrite(capability, Capability, Acc),
	overwrite(capabilities, List_Of_Capabilities, Acc1).


extract_only_need_capabilities(Generic, List_Of_Capabilities) ->
	lists:flatten([extract_one_capabilty(Generic, Capability) || Capability <- List_Of_Capabilities]).
extract_one_capabilty(Generic, {Name, {_Value,_Operator}}) ->
	case lists:keyfind(Name, 2, Generic) of
 		false -> [];
		Cap -> Cap
	end.

get_capability(Capability_Name, Capabilities) ->
	lists:keyfind(Capability_Name, 2, Capabilities).

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
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
search_by_ua_test() ->
	search_by_ua("Mozilla/5.0 (Linux; U; Android 2.1-update1; de-ch; SonyEricssonX10i Build/2.0.A.0.504) AppleWebKit/530.17 (KHTML, like Gecko) Version/4.0 Mobile Safari/530.17",
				 wurfler_db:find_os_device_id("Android"), new_state()).

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

run_funs_against_list_test() ->
	List_of_para=[{"jpg", {"true", '='}}, {"gif", {"true", '='}},  {"png", {"true", '='}}],
	Funs = create_funs_from_list(List_of_para),
	Capa_List_1 = [{"jpg", {"true", '='}}, {"gif", {"true", '='}}, {"png", {"true", '='}}],
	Caps1 = get_all_capabilities(["generic", "htc_desire_a8181_ver1"]),
	?assertEqual({ok},run_funs_against_list(Funs, extract_only_need_capabilities(Caps1, Capa_List_1), [])),

	List_of_para2=[{"jpg", {"true", '='}}, {"gif", {"true", '='}}, {"png", {"false", '='}}],
	Funs2 = create_funs_from_list(List_of_para2),
	Capa_List_2 = [{"jpg", {"true", '='}}, {"gif", {"true", '='}}, {"png", {"true", '='}}],
	Caps2 = get_all_capabilities(["generic", "htc_desire_a8181_ver1"]),
	?assertEqual({nok},run_funs_against_list(Funs2, extract_only_need_capabilities(Caps2, Capa_List_2), [])).

search_by_capabilities_test() ->
	List=[{"handheldfriendly", {"false", '='}},
	 {"playback_mp4", {"false", '='}},
	 {"playback_wmv", {"none", '='}}],
	Devices = search_by_capabilities(List, "01.01.2011", [],new_state()),
	?assertEqual(1, erlang:length(Devices)).
	
overwrite_capability_test() ->
	Generic = get_generic_capabilities(),
	C = [#capability{name="device_os_version", value="3.0"}, #capability{name="device_os", value="Test"}],
	D = overwrite(capabilities, C, Generic),
	Cap1 = get_capability("device_os_version", D),
	Cap2 = get_capability("device_os", D),
	?assertEqual("3.0", Cap1#capability.value),
	?assertEqual("Test", Cap2#capability.value).

overwrite_capabilities_test() ->
	Generic = wurfler_db:find_capabilities_by_id(devicesTbl, "generic"),
	Acc = overwrite(capabilities, Generic, []),
	Capabilities = wurfler_db:find_capabilities_by_id(devicesTbl, "apple_ipad_ver1"),
	Caps = overwrite(capabilities, Capabilities, Acc),
	Brand = get_capability("brand_name", Caps),
	Model = get_capability("model_name", Caps),
	Is_tablet = get_capability("is_tablet", Caps),
	?assertEqual([], Brand#capability.value),
	?assertEqual("iPad", Model#capability.value),
	?assertEqual("true", Is_tablet#capability.value).

get_devices_for_caps_test() ->
	List=[{"model_name", {"iPad", '='}},{"brand_name", {"Apple", '='}}],
	List_Of_Funs = create_funs_from_list(List),
	Keys = get_keys("iPhone OS", []),
	State = #state{capabilities=extract_only_need_capabilities(get_generic_capabilities(), List)},
	?assertEqual(10, erlang:length(get_devices_for_caps(List_Of_Funs, Keys, State))).
	
optimization_test() ->
	Generic = get_generic_capabilities(),
	C = [{"device_os_version", {"3.0", "="}}, {"device_os", {"Test", "="}}, {"device_os1", {"Test", ">"}}],
	Result = extract_only_need_capabilities(Generic, C),
	?assertEqual(2, erlang:length(Result)).

get_all_capabilities_test() ->
	Caps = get_all_capabilities(["generic","generic_xhtml","apple_generic","apple_ipad_ver1"]),
	Model = get_capability("model_name", Caps),
	?assertEqual("iPad", Model#capability.value).












