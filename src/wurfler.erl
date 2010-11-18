%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
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
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).
-export([parse_wurfl/1, searchByUA/1, searchByCapabilities/1, searchByDeviceName/1, getAllCapabilities/1, getVersion/0]).
%%-compile([export_all]).
%% ====================================================================
%% Record definition
%% ====================================================================
-record(state, {capabilities}).
%% ====================================================================
%% External functions
%% ====================================================================
parse_wurfl(Filename) ->
	gen_server:cast(?MODULE, {parse_wurfl, Filename}).

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
	ets:new(deviceTbl, [named_table,public,{keypos, #device.id}]),
	wurfler:parse_wurfl("test/wurfltest.xml"),
    {ok, #state{capabilities=[]}}.

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
	Result=search_by_capabilities(Capabilities),
    {reply, Result, State};
handle_call({search_by_ua, Capabilities}, _From, State) ->
	Result=search_by_ua(Capabilities),
    {reply, Result, State};
handle_call({search_by_device_id, DeviceName}, _From, State) ->
	Result=search_by_device_id(DeviceName),
    {reply, Result, State};
handle_call({get_all_capabilities, DeviceName}, _From, State) ->
	Result=get_all_capabilities(DeviceName, State),
    {reply, Result, State};
handle_call({version}, _From, State) ->
    {reply, "0.1", State}.
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({parse_wurfl, Filename}, State) ->
	create_model(Filename),
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
search_by_device_id(DeviceName)->	
	case ets:lookup(deviceTbl, DeviceName) of
		[Device] -> Device;
		[] -> []
	end.

search_by_ua(UserAgent)->
	case ets:match_object(deviceTbl, #device{id='_',user_agent=UserAgent, _='_'}) of
		[Device] -> Device#device.id;
		[] -> []
	end.

	
get_all_capabilities(DeviceName, #state{capabilities=Caps}) ->
	Result = ets:match(deviceTbl, #device{id=DeviceName, _='_', fall_back='$1', groups='$2', _='_'}),	
	Capabilities = get_all_capabilities(Result, #state{capabilities=[]}),
	case extract_fall_back(Result) of
		[] -> Capabilities;
		"root" -> Capabilities;
		Fall_Back -> get_all_capabilities(Fall_Back, #state{capabilities=[Caps|Capabilities]})
	end;

get_all_capabilities([], #state{capabilities=Caps})->
	#state{capabilities=Caps};
get_all_capabilities([Result], #state{capabilities=Caps})->	
	Capabilities = extract_capabilities(Result, []),
	NewAcc = extract_capabilities(Caps, Capabilities),
	#state{capabilities=NewAcc}.
	
handle_device(Device)->
	Fall_back = extract_fall_back(Device),	
	Capabilities = extract_capabilities(Device, []),
	{Fall_back, Capabilities}.

extract_fall_back([]) ->
	[];
extract_fall_back(Device) ->
	lists:nth(1, Device).

extract_capabilities([], Acc) ->
	Acc;
extract_capabilities(Device, Acc) ->
	Groups = lists:nth(2, Device),
	[concat_capabilites(Acc, Group#group.capabilites) || Group <- Groups].
concat_capabilites(Acc, List2)->
	lists:append(Acc, List2).
	
search_by_capabilities(Capabilities)->
	ok.

create_model(Filename)->
	Xml = parse(Filename),
	XPath = "/wurfl/devices/device",
	DevicesXml = xmerl_xpath:string (XPath, Xml),
	Devices=process_devices(DevicesXml),
	store_devices(Devices).

parse(Filename) ->
	case xmerl_scan:file(Filename) of
	{Xml,_Rest} -> Xml;
	Error -> error_logger:info_msg("Some other result ~p~n",[Error]),
			 undefined
	end.

get_devices(Xml)->
	XPath = "/wurfl/devices/device",
	Devices = xmerl_xpath:string (XPath, Xml),
	process_devices(Devices).

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
	create_device(Device_Attributes, Groups).

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
	
store_devices(Devices) ->
	ets:insert(deviceTbl, Devices).
%% --------------------------------------------------------------------
%%% Test utility functions
%% --------------------------------------------------------------------
%% 	test_setup() ->
%% 		ets:new(device, [named_table, {keypos, #device.id}]),
%% 		create_model("test/wurfltest.xml").
%% 
%% 	test_cleanup() ->
%% 		ets:delete(?DEVICE),
%% 		ok.
	
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
%% my_test()->
%% 	{setup,
%% 	 	fun test_setup/0,
%% 	 	fun test_cleanup/0,
%% 	 	[fun search_by_device_id_test/0]
%% 	}.

%% search_by_device_id_test()->
%% 	test_setup(),
%% 	Device = search_by_device_id("hackingtosh"),
%% 	?assertEqual("hackingtosh", Device#device.id),
%% 	test_cleanup().
	
search_by_ua_test()->
	Device=wurfler:search_by_ua("rocker_ua"),
	?assertEqual("hackingtosh", Device#device.id).
	

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
	A=[["generic", [#group{id = "j2me", capabilites = [#capability{name = "myVersion",value = "6.1"}, #capability{name = "myProfile",value = "1.1"}]}]]],
	[pro(X) || X <- A].
	

pro(X) ->
	Groups = lists:flatten(lists:nth(2, X)),
	lists:foldl(fun(G,Result) -> io:format("1..~p~n", [G#group.capabilites])end, [], Groups).

