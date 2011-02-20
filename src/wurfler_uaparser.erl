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
-module(wurfler_uaparser).

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
-export([parse/1]).

%% ====================================================================
%% External functions
%% ====================================================================
parse(User_Agent) ->
	gen_server:call(?MODULE, {parse, User_Agent}).
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
handle_call({parse, User_Agent}, _From, State) ->
	Result = parse_useragent(User_Agent),
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
parse_useragent(User_Agent) ->
	Keys = wurfler_db:get_all_keys(os_device_id),
	parse_useragent(Keys, User_Agent, []).

parse_useragent([], _User_Agent, Acc) ->
	Acc;
parse_useragent([Key|Keys], User_Agent, Acc) ->
	[#os_device_id{os_reg = OS_Reg, device_ids = Device_Ids}] = wurfler_db:find_os_device_id(Key),
	case re:run(User_Agent, OS_Reg) of 
		{match, _} -> io:format("Match found for OS : ~p , ~p~n", [Key, OS_Reg]), 
					  Acc1 = lists:append(Device_Ids, Acc),
				  	  parse_useragent(Keys, User_Agent, Acc1);
		nomatch -> %%error_logger:info_msg("No match for OS : ~p~n", [Key]),
				   parse_useragent(Keys, User_Agent, Acc)
	end.

search_ua(_User_Agent, []) ->
	[];							 
search_ua(User_Agent, Device_Ids) ->
	error_logger:info_msg("start search ua with ~p~n",[Device_Ids]),
	wurfler_search:searchByUA(User_Agent, Device_Ids).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
short_test() ->
	UA = "Mozilla/5.0 (Linux; U; Android 1.1; en-us; T-Mobile G1 Build/PLAT-RC33) AppleWebKit/525.10+ (KHTML, like Gecko) Version/3.0.4 Mobile Safari/523.12.2",
	?assertMatch(_,parse_useragent(UA)).


