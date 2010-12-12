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
-export([searchByUA/1, searchByCapabilities/1, searchByDeviceName/1, getAllCapabilities/1, getVersion/0]).

-compile([export_all]).
%% ====================================================================
%% Record definition
%% ====================================================================
-record(state, {devices=[], groups=[], capabilities=[], modell=[]}).
%% ====================================================================
%% External functions
%% ====================================================================
import_wurfl(Filename) ->
	gen_server:cast(?MODULE, {import_wurfl, Filename}).
searchByCapabilities(Capabilities) ->
	gen_server:call(?MODULE, {search_by_capabilities, Capabilities}).
searchByUA(UserAgent)->
	gen_server:call(?MODULE, {search_by_ua, UserAgent}).
searchByDeviceName(DeviceName) ->
	gen_server:call(?MODULE, {search_by_device_id, DeviceName}).
getAllCapabilities(DeviceName)->
	gen_server:call(?MODULE, {get_all_capabilities, DeviceName}).
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
handle_call({search_by_capabilities, Capabilities}, _From, State) ->
	Result=search_by_capabilities(Capabilities, State),
    {reply, Result#state.devices, State};
handle_call({search_by_ua, Capabilities}, _From, State) ->
	Result=search_by_ua(Capabilities, State),
    {reply, Result, State};
handle_call({search_by_device_id, DeviceName}, _From, _State) ->
	Result=search_by_device_id(DeviceName),
    {reply, Result, new_state()};
handle_call({get_all_capabilities, DeviceName}, _From, State) ->
	{ok, Result}=get_all_capabilities(DeviceName, State),
    {reply, Result#state.capabilities, new_state()};
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
		[Device] -> Device
	end.
search_by_capabilities(Capabilities, State) ->
	List_Of_Funs=create_funs_from_list(Capabilities),
	Keys = wurfler_db:get_all_keys(devicesTbl),
	get_devices_for_caps(List_Of_Funs, Keys, State).

get_devices_for_caps(_List_Of_Funs, [], State) ->
	State#state{devices=create_devices(State#state.devices)};
	
get_devices_for_caps(List_Of_Funs, [Key|Keys], State)->
	Device = search_by_device_id(Key),
	{ok,#state{capabilities=Caps}} = get_all_capabilities(Device#device.id, State#state{capabilities=[]}),
	case run_funs_against_list(List_Of_Funs, Caps) of
		{ok} -> get_devices_for_caps(List_Of_Funs, Keys,State#state{devices=[create_device(Device)|State#state.devices]});
		{nok} -> get_devices_for_caps(List_Of_Funs, Keys, State)
	end.

create_device(#device{id=Id, brand_name=Brand_name, model_name=Model_name}) ->
	{'device', [{id, Id}, {model_name,Model_name}, {brand_name,Brand_name}], []}.
create_devices(Devices)->
	[{'devices', [], Devices}].

get_all_groups([], #state{groups=Groups}) ->
	{ok, #state{groups=Groups}};
get_all_groups("root", #state{groups=Groups}) ->
	{ok, #state{groups=Groups}};
get_all_groups("generic", #state{groups=Groups}) ->
	{ok, #state{groups=Groups}};
get_all_groups(DeviceName, #state{groups=AllGroups}) ->
	[{Fall_back, Groups}] = wurfler_db:find_groups_by_id(devicesTbl, DeviceName),
	get_all_groups(Fall_back, #state{groups=lists:append(AllGroups,Groups)}).

get_all_capabilities([], #state{capabilities=Caps}) ->
	{ok, #state{capabilities=Caps}};
get_all_capabilities("root", #state{capabilities=Caps}) ->
	{ok, #state{capabilities=Caps}};
get_all_capabilities("generic", #state{capabilities=Caps}) ->
 	{ok, #state{capabilities=Caps}};
get_all_capabilities(DeviceName, #state{capabilities=Caps}) ->
	{Fall_back, Capabilities} = wurfler_db:find_capabilities_by_id(devicesTbl, DeviceName),	
	get_all_capabilities(Fall_back, #state{capabilities=lists:append(Caps,Capabilities)}).

%%------------------------------------------------------------------------------
%% Here i can optimize the create_fun stuff.
%% Perhaps i will use erl_scan and co.
%%------------------------------------------------------------------------------
create_fun(CheckName, CheckValue, '==')->
	fun(Name, Value) ->
		case Name of
			CheckName ->  case Value == CheckValue of
							 true -> {ok};
							 false -> {nok}
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
%% Run all funs for one capability
%% Only if all funs return ok, its ok
and_cond([Fun|Funs], {CheckName, CheckValue}) ->
    case Fun(CheckName, CheckValue) of
		{ok} -> and_cond({ok});
		{nok} -> and_cond({nok});
        {continue} -> and_cond(Funs, {CheckName, CheckValue})
    end;
and_cond([], {_CheckName, _CheckValue}) -> 
	{continue}.
and_cond(ReturnValue) ->
	ReturnValue.	

create_funs_from_list(List) ->
	[create_fun(Name, Value, Operator) || {Name, {Value, Operator}} <- List].

run_funs_against_list(List_Of_Funs, [#capability{name=CheckName, value=CheckValue}|List_Of_Caps]) ->
	case and_cond(List_Of_Funs, {CheckName, CheckValue}) of
		{continue} -> run_funs_against_list(List_Of_Funs, List_Of_Caps);
		{ok} -> run_funs_against_list(List_Of_Funs, List_Of_Caps);
		{nok} -> {nok}
	end;
	
run_funs_against_list(_List_Of_Funs, []) ->
	{ok}.
	
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
create_function_test() ->
	A=create_fun("test", "123", '=='),
	?assertEqual({ok}, A("test", "123")),
	?assertEqual({nok}, A("test", "12")),
	?assertEqual({continue}, A("unknown", "123")),
	
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
	List=[{"handheldfriendly", {"false", '=='}},
	 {"playback_mp4", {"false", '=='}},
	 {"playback_wmv", {"none", '=='}}],
	?assertEqual(3, erlang:length(create_funs_from_list(List))).

run_funs_against_list_test()->
	List_of_para=[{"handheldfriendly", {"true", '=='}},
	 {"playback_mp4", {"false", '=='}},
	 {"playback_wmv", {"none", '=='}}],
	?assertEqual({ok},run_funs_against_list(create_funs_from_list(List_of_para), wurfler:getAllCapabilities("generic"))),
	List_of_para1=[{"handheldfriendly", {"false", '=='}},
	 {"playback_mp4", {"false", '=='}},
	 {"playback_wmv", {"none", '=='}}],
	?assertEqual({ok},run_funs_against_list(create_funs_from_list(List_of_para1), wurfler:getAllCapabilities("generic"))).
	
	
search_by_capabilities_test() ->
	List=[{"handheldfriendly", {"false", '=='}},
	 {"playback_mp4", {"false", '=='}},
	 {"playback_wmv", {"none", '=='}}],
	search_by_capabilities(List, new_state()).

search_by_ua_test()->
	Device = search_by_ua("Mozilla/4.1 (compatible; MSIE 5.0; Symbian OS; Nokia 7610", wurfler:new_state()),
	?assertEqual("opera_nokia_7610_ver1", Device#device.id).
	
	
search_by_device_id_test() ->
	?assertEqual([],search_by_device_id("unknown")).






	



