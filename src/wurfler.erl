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
-export([parse_wurfl/1]).
-compile([export_all]).

-record(state, {devices=[]}).

%% ====================================================================
%% External functions
%% ====================================================================
parse_wurfl(Filename) ->
	gen_server:cast(?MODULE, {parse, Filename}).
search(Capabilities) ->
	gen_server:cast(?MODULE, {search, Capabilities}).
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
    {ok, #state{devices=create_model("test/wurfl.xml")}}.

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
handle_call({search, Capabilities}, _From, State) ->
	Result=search(Capabilities),
    {reply, Result, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({parse, Filename}, _State) ->
	NewState = #state{devices=create_model(Filename)},
    {noreply, NewState}.

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
search(Capabilities)->
	ok.

create_model(Filename)->
	Xml = parse(Filename),
	get_devices(Xml).

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
	process_capability_attributes(Attributes).

process_capability_attributes(Attributes)->
	[process_capability_attribute(Attribute) || Attribute <-Attributes].

process_capability_attribute(Attribute)->
	case Attribute of
		{xmlAttribute,name,_,_,_,_,_,_,Name,_} -> {name, Name};
		{xmlAttribute,value,_,_,_,_,_,_,Value,_} -> {value, Value}
	end.

get_groups(Device)->
	XPath = "group",
	Groups = xmerl_xpath:string (XPath, Device),
	process_groups(Groups).

process_groups(Groups)->
	[process_group(Group) || Group <- Groups].

process_group(Group)->	
	Capabilities = get_capabilities(Group),
	{xmlElement,group,_,_,_,_,_,Attributes,_,_,_,_} = Group,
	Group_Attributes = process_group_attributes(Attributes),
	create_group(Group_Attributes, Capabilities).

process_group_attributes(Attributes)->
	[process_group_attribute(Attribute) || Attribute <- Attributes].

process_group_attribute(Attribute)->
	case Attribute of
		{xmlAttribute,id,_,_,_,_,_,_,Id,_} -> {id, Id}
	end.
  
process_devices(Devices) ->
	[process_device(Device)|| Device <- Devices].

process_device(Device) ->
	Groups = get_groups(Device),
	{xmlElement,device,_,[],_,[_,_],_,Attributes,_,_,_,_} = Device,
	Device_Attributes=process_device_attributes(Attributes),
	create_device(Device_Attributes, Groups).
	
process_device_attributes(Attributes)->
	[process_device_attribute(Attribute) || Attribute <- Attributes].

process_device_attribute(Attribute)->
	case Attribute of
		{xmlAttribute,id,_,_,_,_,_,_,Id,_} -> {id, Id};
		{xmlAttribute,user_agent,_,_,_,_,_,_,User_agent,_} -> {user_agent, User_agent};
		{xmlAttribute,fall_back,_,_,_,_,_,_,Fall_back,_} -> {fall_back, Fall_back};
		{xmlAttribute,actual_device_root,_,_,_,_,_,_,Fall_back,_} -> {actual_device_root, Fall_back}
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
	#capability{name=proplists:get_value(name, Attributes), value=proplistes:get_value(value, Attributes)}.
	
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
create_model_test()->
	create_model("test/wurfltest.xml").

parse_test() ->
	Filename = "test/wurfltest.xml",
	Xml = parse(Filename),
	io:format("Xml ~p~n", [Xml]).
	%%?assertEqual(2, erlang:length(Xml)).

get_devices_test()->
	Xml = parse("test/wurfltest.xml"),
	?assertEqual(3,erlang:length(get_devices(Xml))).

process_devices_test()->
	Xml = parse("test/wurfltest.xml"),
	Devices = get_devices(Xml),
	process_devices(Devices).


process_device_test()->
D={xmlElement,device,device,[],
           {xmlNamespace,[],[]},
           [{devices,4},{wurfl,1}],
           6,
           [{xmlAttribute,id,[],[],[],[],1,[],"hackingtosh",false},
            {xmlAttribute,user_agent,[],[],[],[],2,[],"CDM-8150/P15 UP.Browser/4.1.26c4",false},
            {xmlAttribute,fall_back,[],[],[],[],3,[],"rocker",false}]},


{xmlElement,device,_,[],_,[_,_],_,Attributes} = D,
L=process_device_attributes(Attributes),
io:format("A : ~p~n", [L]).

get_groups_test() ->
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

process_groups_test() ->
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
	



