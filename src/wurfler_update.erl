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
-module(wurfler_update).

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
-export([create_device/1, update_device/1, delete_device/1]).

%% ====================================================================
%% External functions
%% ====================================================================
create_device(Device) ->
	gen_server:cast(?MODULE, {create_device, Device}).
update_device(Device) ->
	gen_server:cast(?MODULE, {update_device, Device}).
delete_device(Device) ->
	gen_server:cast(?MODULE, {delete_device, Device}).
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
handle_cast({create_device, Device}, State) ->
	error_logger:info_msg("create device : ~p~n", [Device]),
	#device{id=Id} = read_device(Device),
	case lists:flatten([wurfler:check_device(Capabilities, [Id]) || Capabilities <- wurfler_db:get_all_keys(capabilities_devices)]) of
		[] -> error_logger:info_msg("nothing to do for device : ~p~n", [Device]);
		Result -> error_logger:info_msg("must update cache for device : ~p~n", [Device]),
				  request_for_capablities(Result)
	end,
    {noreply, State};
handle_cast({update_device, Device}, State) ->
	error_logger:info_msg("update device : ~p~n", [Device]),
	D = read_device(Device),
    {noreply, State};
handle_cast({delete_device, Device}, State) ->
	error_logger:info_msg("delete device : ~p~n", [Device]),
	D = read_device(Device),
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
request_for_capablities([]) ->
	ok;
request_for_capablities([{SetOfCaps, Devices1}|Caps]) ->
	io:format("2... ~p~n", [Devices1]),
	Devices = wurfler:searchByCapabilities(SetOfCaps, ?DEFAULT_TIMESTAMP),
	wurfler_cache:save_caps_devices(SetOfCaps, Devices),
	write_change_set(SetOfCaps, Devices),
	request_for_capablities(Caps).
write_change_set([], _Devices) ->
	error_logger:info_msg("ERROR"),
	ok;
write_change_set(Caps, Devices) ->
	error_logger:info_msg("writing caps and devices : ~p~n~p~n", [Caps,Devices]).
read_device(#device{id=Id}) ->
	wurfler:searchByDeviceName(Id).	
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
insert_new_device_test() ->
	D = #device{id="apple_iphone_ver1"},
	handle_cast({create_device, D}, #state{}).