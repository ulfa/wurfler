%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------

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
-module(wurfler).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/wurfler.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(WURFL_CONFIG, "priv/wurfler.config").
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0]).
-export([import_wurfl/1, searchByUA/1, searchByCapabilities/1, searchByDeviceName/1, getAllCapabilities/1, getVersion/0]).
-export([backup/0, load/0]).

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
backup()->
	gen_server:cast(?MODULE, {backup}).
load()-> 
	gen_server:cast(?MODULE, {load}).
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
	ets:new(deviceTbl, [named_table,public,{keypos, #device.id}]),
	%%wurfler:create_model(get_wurfl_file(?WURFL_CONFIG), new_state()),
	wurfler:import_wurfl_file(get_wurfl_file(?WURFL_CONFIG)),
    {ok, new_state()}.

get_wurfl_file(?WURFL_CONFIG) ->
	{ok, Config} = file:consult(?WURFL_CONFIG),
	proplists:get_value(wurfl_file, Config).
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
handle_cast({import_wurfl, Filename}, State) ->
	%%create_model(Filename, State),
	import_wurfl_file(Filename),
    {noreply, State};
handle_cast({backup}, State) ->
	backup_table(),
    {noreply, State};
handle_cast({load}, State) ->
	load_table(),
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
	case ets:lookup(deviceTbl, DeviceName) of
		[] -> [];
		[Device] -> Device
	end.

search_by_ua(UserAgent, State)->
	case ets:match_object(deviceTbl, #device{id='_',user_agent=UserAgent, _='_'}) of
		[] -> [];
		[Device] -> Device,
					case get_all_groups(Device#device.id, State) of
						{ok,  #state{groups=Groups}} -> Device#device{groups=Groups};
						_ -> {error, []}
					end
	end.
search_by_capabilities(Capabilities, State) ->
	io:format("State ~n~p", [erlang:length(State#state.modell)]),
	List_Of_Funs=create_funs_from_list(Capabilities),
	NextKey = ets:first(deviceTbl),
	get_devices_for_caps(List_Of_Funs, NextKey, State).

get_devices_for_caps(_List_Of_Funs, '$end_of_table', State) ->
	State#state{devices=create_devices(State#state.devices)};
	
get_devices_for_caps(List_Of_Funs, Key, State)->
	NextKey = ets:next(deviceTbl, Key),
	Device = search_by_device_id(Key),
	{ok,#state{capabilities=Caps}} = get_all_capabilities(Device#device.id, State#state{capabilities=[]}),
	case run_funs_against_list(List_Of_Funs, Caps) of
		{ok} -> get_devices_for_caps(List_Of_Funs, NextKey,State#state{devices=[create_device(Device)|State#state.devices]});
		{nok} -> get_devices_for_caps(List_Of_Funs, NextKey, State)
	end.

create_device(#device{id=Id}) ->
	{'device', [{id, Id}, {model_name,[]}, {brand_name,[]}], []}.
create_devices(Devices)->
	[{'devices', [], Devices}].

get_all_groups([], #state{groups=Groups}) ->
	{ok, #state{groups=Groups}};
get_all_groups("root", #state{groups=Groups}) ->
	{ok, #state{groups=Groups}};
get_all_groups("generic", #state{groups=Groups}) ->
	{ok, #state{groups=Groups}};
get_all_groups(DeviceName, #state{groups=AllGroups}) ->
	[[Fall_back, Groups]] = ets:match(deviceTbl, #device{id=DeviceName, _='_', fall_back='$1', groups='$2', _='_'}),
	get_all_groups(Fall_back, #state{groups=lists:append(AllGroups,Groups)}).

get_all_capabilities([], #state{capabilities=Caps}) ->
	{ok, #state{capabilities=Caps}};
get_all_capabilities("root", #state{capabilities=Caps}) ->
	{ok, #state{capabilities=Caps}};
get_all_capabilities("generic", #state{capabilities=Caps}) ->
 	{ok, #state{capabilities=Caps}};
get_all_capabilities(DeviceName, #state{capabilities=Caps}) ->
	[[Fall_back, Groups]] = ets:match(deviceTbl, #device{id=DeviceName, _='_', fall_back='$1', groups='$2', _='_'}),
	Capabilities = lists:append(lists:foldl(fun(Group,Result) -> [Group#group.capabilites|Result] end, [], Groups)),
	get_all_capabilities(Fall_back, #state{capabilities=lists:append(Caps,Capabilities)}).

create_model(Filename, _State)->
	Xml = parse(Filename),
	XPath = "/wurfl/devices/device",
	DevicesXml = xmerl_xpath:string (XPath, Xml),
	Devices = process_devices(DevicesXml),
	store_devices(Devices),
	error_logger:info_msg("loaded model~n").

import_wurfl_file(Filename) ->
	Xml = parse(Filename),
	DevicesXml = xmerl_xpath:string ("/wurfl/devices/device", Xml),
	process_devices(DevicesXml).
	
parse(Filename) ->
	case xmerl_scan:file(Filename) of
	{Xml,_Rest} -> Xml;
	Error -> error_logger:error_info("Some other result ~p~n",[Error]),
			 undefined
	end.

get_capabilities(Group)->
	XPath = "capability",
	Capabilities = xmerl_xpath:string (XPath, Group),
	process_capabilities(Capabilities).
	
process_capabilities(Capabilities)->
	[process_capability(Capability) || Capability <- Capabilities].

process_capability(Capability)->
	{xmlElement,capability,_,_,_,_,_,Attributes,_,_,_,_} = Capability,
	Capability_Attributes=process_attributes(capability, Attributes),
	create_capability(Capability_Attributes).

get_groups(Device)->
	XPath = "group",
	Groups = xmerl_xpath:string (XPath, Device),
	process_groups(Groups).

process_groups(Groups)->
	[process_group(Group) || Group <- Groups].
process_devices(Devices) ->
	[process_device(Device)|| Device <- Devices].

process_group(Group)->	
	Capabilities = get_capabilities(Group),
	{xmlElement,group,_,_,_,_,_,Attributes,_,_,_,_} = Group,
	Group_Attributes = process_attributes(group,Attributes),
	create_group(Group_Attributes, Capabilities).

process_device(Device) ->
	Groups = get_groups(Device),
	{xmlElement,device,_,[],_,[_,_],_,Attributes,_,_,_,_} = Device,
	Device_Attributes=process_attributes(device, Attributes),
	Device_Record = create_device(Device_Attributes, Groups),
	store_devices(Device_Record),
	Device_Record.
	
process_attributes(group, Attributes)->
	[process_attribute(group, Attribute) || Attribute <- Attributes];
process_attributes(device, Attributes)->
	[process_attribute(device, Attribute) || Attribute <- Attributes];
process_attributes(capability, Attributes)->
	[process_attribute(capability,Attribute) || Attribute <-Attributes].

process_attribute(group, Attribute) ->
	case Attribute of
		{xmlAttribute,id,_,_,_,_,_,_,Id,_} -> {id, Id}
	end;
process_attribute(device, Attribute)->
	case Attribute of
		{xmlAttribute,id,_,_,_,_,_,_,Id,_} -> {id, Id};
		{xmlAttribute,user_agent,_,_,_,_,_,_,User_agent,_} -> {user_agent, User_agent};
		{xmlAttribute,fall_back,_,_,_,_,_,_,Fall_back,_} -> {fall_back, Fall_back};
		{xmlAttribute,actual_device_root,_,_,_,_,_,_,Fall_back,_} -> {actual_device_root, Fall_back}
	end;
process_attribute(capability, Attribute)->
	case Attribute of
		{xmlAttribute,name,_,_,_,_,_,_,Name,_} -> {name, Name};
		{xmlAttribute,value,_,_,_,_,_,_,Value,_} -> {value, Value}
	end.

create_device(Attributes, Groups)->
	#device{id=proplists:get_value(id, Attributes), 
			user_agent=proplists:get_value(user_agent, Attributes),
			actual_device_root=proplists:get_value(actual_device_root, Attributes),
			fall_back=proplists:get_value(fall_back, Attributes),
			groups=Groups}.

create_group(Attributes, Capabilities) ->
	#group{id=proplists:get_value(id, Attributes), capabilites=Capabilities}.

create_capability(Attributes) ->
	#capability{name=proplists:get_value(name, Attributes), value=proplists:get_value(value, Attributes)}.
	
store_devices(Device) ->
	ets:insert(deviceTbl, Device),
	wurfler_db:save_device(devicesTbl, Device).

backup_table() ->
	ets:tab2file(deviceTbl, "./backup/wurfl.tbl").
load_table() ->
	ets:file2tab("./backup/wurfl.tbl", [{verify,true}]).

create_fun(CheckName, CheckValue, '==')->
	fun(Name, Value) ->
		case Name of
			CheckName ->  case Value of
							 CheckValue -> {ok};
							 _ -> {nok}
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
	
run_funs_against_list(List_Of_Funs, []) ->
	{ok}.
	
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------

create_function_test() ->
	A=create_fun("test", "123", '=='),
	?assertEqual({ok}, A("test", "123")),
	B=create_fun("test", "123", '/='),
	?assertEqual({ok}, B("test", "234")),
	?assertEqual({nok}, B("test" , "123")),
	?assertEqual({continue}, B("test1" , "234")),
	C=create_fun("test", 123, '>'),
	?assertEqual({ok}, C("test", 234)),
	?assertEqual({continue}, C("test1", 234)),
	D=create_fun("test", 123, '<'),
	?assertEqual({ok}, D("test", 24)).
	
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
	Device = search_by_ua("rocker_ua", #state{groups=[], capabilities=[]}),
	?assertEqual("rocker", Device#device.id).
	
process_device_test1()->
	Result = ets:match(device, #device{id='$1',_='_'}),
	?assertEqual("hackingtosh", lists:flatten(Result)).

parse_test1() ->
	Filename = "test/wurfltest.xml",
	Xml = parse(Filename),
	io:format("Xml ~p~n", [Xml]).
	%%?assertEqual(2, erlang:length(Xml)).


get_groups_test1() ->
	A={xmlElement,device,device,[],
           {xmlNamespace,[],[]},
           [{devices,4},{wurfl,1}],
           6,
           [{xmlAttribute,id,[],[],[],[],1,[],"hackingtosh",false},
            {xmlAttribute,user_agent,[],[],[],[],2,[],
                "CDM-8150/P15 UP.Browser/4.1.26c4",false},
            {xmlAttribute,fall_back,[],[],[],[],3,[],"rocker",false}],
           [{xmlText,[{device,6},{devices,4},{wurfl,1}],1,[],"\n\t\t\t",text},
            {xmlElement,group,group,[],
                {xmlNamespace,[],[]},
                [{device,6},{devices,4},{wurfl,1}],
                2,
                [{xmlAttribute,id,[],[],[],[],1,[],"sis",false}],
                [{xmlText,
                     [{group,2},{device,6},{devices,4},{wurfl,1}],
                     1,[],"\n\t\t\t\t",text},
                 {xmlElement,capability,capability,[],
                     {xmlNamespace,[],[]},
                     [{group,2},{device,6},{devices,4},{wurfl,1}],
                     2,
                     [{xmlAttribute,name,[],[],[],[],1,[],
                          "mobile_browser_version",false},
                      {xmlAttribute,value,[],[],[],[],2,[],"6.1",false}],
                     [],[],undefined,undeclared},
                 {xmlText,
                     [{group,2},{device,6},{devices,4},{wurfl,1}],
                     3,[],"\n\t\t\t    ",text},
                 {xmlElement,capability,capability,[],
                     {xmlNamespace,[],[]},
                     [{group,2},{device,6},{devices,4},{wurfl,1}],
                     4,
                     [{xmlAttribute,name,[],[],[],[],1,[],"release_date",
                          false},
                      {xmlAttribute,value,[],[],[],[],2,[],"2002_august",
                          false}],
                     [],[],undefined,undeclared},
                 {xmlText,
                     [{group,2},{device,6},{devices,4},{wurfl,1}],
                     5,[],"\n\t\t\t",text}],
                [],undefined,undeclared},
            {xmlText,[{device,6},{devices,4},{wurfl,1}],3,[],"\t\n\t\t",text}],
           [],undefined,undeclared},

	
D=get_groups(A),
	
io:format("~p~n", [D]).

process_groups_test1() ->
	G={xmlElement,group,group,[], {xmlNamespace,[],[]}, [{device,6},{devices,4},{wurfl,1}],2,
          [{xmlAttribute,id,[],[],[],[],1,[],"sis",false}],
          [{xmlText,
               [{group,2},{device,6},{devices,4},{wurfl,1}],
               1,[],"\n\t\t\t\t",text},
           {xmlElement,capability,capability,[],
               {xmlNamespace,[],[]},
               [{group,2},{device,6},{devices,4},{wurfl,1}],
               2,
               [{xmlAttribute,name,[],[],[],[],1,[],"mobile_browser_version",
                    false},
                {xmlAttribute,value,[],[],[],[],2,[],"6.1",false}],
               [],[],undefined,undeclared},
           {xmlText,
               [{group,2},{device,6},{devices,4},{wurfl,1}],
               3,[],"\n\t\t\t    ",text},
           {xmlElement,capability,capability,[],
               {xmlNamespace,[],[]},
               [{group,2},{device,6},{devices,4},{wurfl,1}],
               4,
               [{xmlAttribute,name,[],[],[],[],1,[],"release_date",false},
                {xmlAttribute,value,[],[],[],[],2,[],"2002_august",false}],
               [],[],undefined,undeclared},
           {xmlText,
               [{group,2},{device,6},{devices,4},{wurfl,1}],
               5,[],"\n\t\t\t",text}],
          [],undefined,undeclared},

{xmlElement,group,_,_,_,_,_,Attributes,_,_,_,_} = G,
	Attributes.
	
process_list_test()->
A=	[{group,"j2me",
                 [{capability,"myVersion","6.1"},
                  {capability,"myProfile","1.1"}]},
     {group,"j2me1",
                 [{capability,"myVersion1","6.1"},
                  {capability,"myProfile1","1.1"}]}],

	B=lists:foldl(fun(G,Result) -> [G#group.capabilites|Result] end, [], A),
	lists:append(B).

process_device_test()->
Groups=	[{group,"j2me",
                 [{capability,"myVersion","6.1"},
                  {capability,"myProfile","1.1"}]},
     {group,"j2me1",
                 [{capability,"myVersion1","6.1"},
                  {capability,"myProfile1","1.1"}]}],


	lists:foldl(fun(Group,Result) ->						 
						  [Caps || Caps <- Group#group.capabilites ]
				  end, [], Groups).

	
get_wurfl_file_test() ->
	?assertEqual("test/wurfltest.xml", get_wurfl_file(?WURFL_CONFIG)).

	







	



