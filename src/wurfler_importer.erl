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
-module(wurfler_importer).

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
-export([import_wurfl/1]).
-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
import_wurfl(Filename) ->
	gen_server:cast(?MODULE, {import_wurfl, Filename}).

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
    {ok, #state{}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({import_wurfl, Filename}, State) ->
	import_wurfl_file(Filename),
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
import_wurfl_file(Filename) ->
	error_logger:info_msg("import started : ~p~n" ,[Filename]),
	Xml = parse(Filename),
	DevicesXml = xmerl_xpath:string ("/wurfl/devices/device", Xml),
	D=process_devices(DevicesXml),
	error_logger:info_msg("import finished : ~p~n" ,[erlang:length(D)]).

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
	wurfler_db:save_device(devicesTbl, Device).

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------

