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
-module(wurfler_cache).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("../include/wurfler.hrl").
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).
-export([save_caps_devices/3, read_caps_devices/1, clear/0]).

%% ====================================================================
%% External functions
%% ====================================================================
save_caps_devices(Caps, Devices, Key) ->
	gen_server:cast(?MODULE, {save_caps_devices, Caps, Devices, Key}).
read_caps_devices(Caps) ->
	gen_server:call(?MODULE, {read_caps_devices, Caps}).
clear() ->
	gen_server:call(?MODULE, {clear}).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {}).
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
handle_call({read_caps_devices, Caps}, _From, State) ->
	Caps_Devices=wurfler_db:read_capabilities_devices(Caps),
    {reply, Caps_Devices, State};
handle_call({clear_caps_devices, _Caps}, _From, State) ->
	Result = wurfler_db:clear_capabilities_devices(),
    {reply, Result, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({save_caps_devices, Caps, Devices, []}, State) ->
	case wurfler_db:get_capablities_devices(Caps) of
		[] -> CD = create_capabilities_devices(Caps, Devices, wurfler_date_util:get_uc_time(), wurfler_date_util:get_uc_time());
		Caps_Devices -> case Caps_Devices#capabilities_devices.key of
							[] -> CD = create_capabilities_devices(Caps, Devices, Caps_Devices#capabilities_devices.created, wurfler_date_util:get_uc_time());
							_  -> CD = create_capabilities_devices(Caps, Devices, Caps_Devices#capabilities_devices.key,Caps_Devices#capabilities_devices.created, 
																   wurfler_date_util:get_uc_time()) 
						end
	end,
	save_capabilities_devices(CD),
    {noreply, State};

handle_cast({save_caps_devices, Capabilities, Devices, Key}, State) ->
	case wurfler_db:find_capabilities_device_by_key(Key) of
 		[] -> CD=create_capabilities_devices(Capabilities, Devices, Key, wurfler_date_util:get_uc_time(), wurfler_date_util:get_uc_time());
		Caps_Devices -> case Key =:= Caps_Devices#capabilities_devices.key of
							 true -> CD = create_capabilities_devices(Capabilities, Devices, wurfler_date_util:get_uc_time());
							 false -> error_logger:warning_msg("The key : ~p for Capabilities : ~p are different! ~n", [Key, Caps_Devices#capabilities_devices.capabilities]),
									  CD = create_capabilities_devices(Capabilities, Devices,  wurfler_date_util:get_uc_time())
						end
	end,			
	wurfler_db:save_capabilities_devices(CD),				
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
save_capabilities_devices(Caps_devices) ->
	wurfler_db:save_capabilities_devices(Caps_devices).
create_capabilities_devices(Capabilities, Devices, Key, Created, Lastmodified) ->
	#capabilities_devices{capabilities=Capabilities, devices=Devices, key=Key, created=Created, lastmodified=Lastmodified}.
create_capabilities_devices(Capabilities, Devices, Created, Lastmodified) ->
	#capabilities_devices{capabilities=Capabilities, devices=Devices, created=Created, lastmodified=Lastmodified}.
create_capabilities_devices(Capabilities, Devices, Lastmodified) ->
	#capabilities_devices{capabilities=Capabilities, devices=Devices, lastmodified=Lastmodified}.

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------

