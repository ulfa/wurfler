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
-module(wurfler_etag_cache).

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
-export([start_link/0, start/0]).
-export([lookup/1, put/2, delete/1, clear/0]).
%% ====================================================================
%% External functions
%% ====================================================================
lookup(Key) ->
	gen_server:call(?MODULE, {lookup, Key}).
put(Key, Term) ->
	gen_server:cast(?MODULE, {put, Key, Term}).
delete(Key) ->
	gen_server:cast(?MODULE, {delete, Key}).
clear() ->
	gen_server:cast(?MODULE, {clear}).
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
handle_call({lookup, Key}, _From, State) ->
	case wurfler_db:lookup(Key) of
		[] -> Data = [];
		[#etag_cache{id=Key, term=Data}] -> Data 
	end,
    {reply, Data, State}.
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({delete, Key}, State) ->
	wurfler_db:delete(etag, Key),
    {noreply, State};
handle_cast({put, Key, Term}, State) ->
	wurfler_db:put(Key, Term),
    {noreply, State};
handle_cast({clear}, State) ->
	wurfler_db:clear(),
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
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
put_test()->
	wurfler_etag_cache:put("test", "data"),
	?assertEqual("data", wurfler_etag_cache:lookup("test")).
	
delete_test() ->
	wurfler_etag_cache:put("delete", "data"),
	wurfler_etag_cache:delete("delete"),
	?assertEqual([], wurfler_etag_cache:lookup("delete")).
	
clear_test() ->
	wurfler_etag_cache:put("test_1", "data"),
	wurfler_etag_cache:put("test_2", "data"),
	wurfler_etag_cache:clear(),
	?assertEqual([], wurfler_etag_cache:lookup("test_1")),
	?assertEqual([], wurfler_etag_cache:lookup("test_2")).

setup() ->
	mnesia:start(),
	wurfler_etag_cache:start().
