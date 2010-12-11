% Copyright 2010 Ulf
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
%%% Description : This module polles directories which are configured
%%% in the priv/wurfler.config. By default the poller looks for 
%%% changed wurfl and wurlf_patch files.
%%% Created : 
%%% -------------------------------------------------------------------
-module(wurfler_file_poller).


-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include("../include/wurfler.hrl").

-define(PROPERTY(Key), wurfler_config:get_value(Key)).
%% --------------------------------------------------------------------
%% External exports
%% cc_timer interface which has to implemented by the timer clients
-export([time_triggered/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).
-record(state, {last_poll_time}).

%% ====================================================================
%% External functions
%% ====================================================================
time_triggered([]) ->
	gen_server:cast(?MODULE, {time_triggered, []}).
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
	application:load(erlbuild),
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
	start_timer(?PROPERTY(timer_interval)),
    {ok, #state{last_poll_time=new_poll_time(date(), time())}}.

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
handle_cast(_Request, State) ->	
    {noreply,State}.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({next_run}, State) ->
	NewStates = [get_new_files(Dir, Regex, list_to_atom(Module), State) || [{directory, Dir}, {regex, Regex}, {modul, Module}] <- ?PROPERTY(polling)],
	start_timer(?PROPERTY(timer_interval)),
    {noreply, lists:last(NewStates)}.
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
get_new_files(Directory, Regex, Module, _State=#state{last_poll_time=Last_poll_time}) ->
%% 	error_logger:info_msg("get files since : ~p in : ~p Regex : ~p for : ~p ~n", [Last_poll_time, Directory, Regex, Module]),
    {ok, Files} = file:list_dir(Directory),
	New_state = #state{last_poll_time=new_poll_time(date(), time())},
    FilteredFiles = lists:map(
        fun(X) -> filename:join([Directory,X]) end,
        lists:filter(
            fun(Y) ->
                re:run(Y,get_regex(Regex),[{capture,none}]) == match end,
            Files
        )
    ),
    NewFiles = lists:filter (
        fun(Filename) ->
            {ok, FileInfo} = file:read_file_info(Filename),
            calendar:datetime_to_gregorian_seconds(FileInfo#file_info.mtime) > Last_poll_time
        end,
        FilteredFiles),
	
%% error_logger:info_msg("New Files ~p~n", NewFiles),	
	case erlang:length(NewFiles) of
		0 -> [];
		_ -> send_to_processing(Module, NewFiles)
	end,
    New_state.
	
get_regex(Regex) ->
	{ok,Compiled_Regex} = re:compile(Regex),
	Compiled_Regex.

start_timer(Time) ->
	erlang:send_after(Time, self(), {next_run}).
%% --------------------------------------------------------------------
%%
%% --------------------------------------------------------------------
send_to_processing(_Module, []) ->
	ok;
send_to_processing(Module, [File|Files]) ->
	error_logger:info_msg("proccessing for : ~p with : ~p~n", [Module, File]),
	Module:import(File),
	send_to_processing(Module, Files).
%% --------------------------------------------------------------------
%%% create new poll time
%% --------------------------------------------------------------------
new_poll_time(Date, Time) ->
	calendar:datetime_to_gregorian_seconds({Date, Time}).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
get_new_files_test() ->
	Poll = new_poll_time(date(), time()),
	NewState=get_new_files("./wurfl", "wurfl.xml$", wurfler_importer, #state{last_poll_time=Poll}),
 	?assert(NewState#state.last_poll_time >= Poll).


