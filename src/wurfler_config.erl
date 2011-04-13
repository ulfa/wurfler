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
-module(wurfler_config).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
-define(WURFL_CONFIG, filename:join([code:priv_dir(wurflerservice), "wurfler.config"])).

%%-define(WURFL_CONFIG, "priv/wurfler.config").
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).
-export([start/0]).
-export([get_value/1, reload/0]).
-compile([export_all]).
-record(state, {config}).

%% ====================================================================
%% External functions
%% ====================================================================
get_value(Key) ->
	gen_server:call(?MODULE, {get_value, Key}).
reload() ->
	gen_server:call(?MODULE, {reload}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link([Config_File]) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config_File], []).

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
 	{ok, [Config]} = file:consult(?WURFL_CONFIG),
	{ok, #state{config=Config}};

init([Config_File]) ->
 	{ok, [Config]} = file:consult(Config_File),
	{ok, #state{config=Config}}.

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
handle_call({get_value, Key}, _From, State=#state{config=Config})->
	Value = proplists:get_value(Key, Config),
    {reply,Value, State};

handle_call({reload}, _From, _State)->
	{ok, [Config]} = file:consult(?WURFL_CONFIG),
    {reply, ok, #state{config=Config}}.
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
 -ifdef(EUNIT).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
get_value_test_() ->
	{setup, 
	 	fun() -> setup() end,
     	fun(_) -> cleanup([]) end,
	 	fun() ->
			[?_assertEqual(5000, wurfler_config:get_value(only_for_testing)),
			 ?_assert(undefined =:= wurfler_config:get_value(not_available))]
	 	end
	 }.

setup() ->
 	wurfler_config:start_link(["priv/wurfler.config"]).

cleanup(_) ->
    ok = wurfler_config:terminate([],[]).

-endif.