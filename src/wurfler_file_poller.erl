%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%%
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
	{Directory, Compiled_Regex} = get_parameter(),
	{Files, NewState} = get_new_files(Directory, Compiled_Regex, State),
	case erlang:length(Files) of
		0 -> [];
		_ -> error_logger:info_msg("Files found : ~p ~n", [Files]),
			 send_to_processing(Files)
	end,

	start_timer(?PROPERTY(timer_interval)),
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
get_new_files(Directory, Compiled_Regex, _State=#state{last_poll_time=Last_poll_time}) ->
	%%error_logger:info_msg("get files since : ~p~n", [Last_poll_time]),
    {ok, Files} = file:list_dir(Directory),
	New_state = #state{last_poll_time=new_poll_time(date(), time())},
    FilteredFiles = lists:map(
        fun(X) -> filename:join([Directory,X]) end,
        lists:filter(
            fun(Y) ->
                re:run(Y,Compiled_Regex,[{capture,none}]) == match end,
            Files
        )
    ),
    NewFiles = lists:filter (
        fun(Filename) ->
            {ok, FileInfo} = file:read_file_info(Filename),
            calendar:datetime_to_gregorian_seconds(FileInfo#file_info.mtime) > Last_poll_time
        end,
        FilteredFiles
    ),				   
    {NewFiles, New_state}.
	
get_parameter() ->
	Directory = ?PROPERTY(polling_dir),
	Regex = ?PROPERTY(files_regex),
	{ok,Compiled_Regex} = re:compile(Regex),
	{Directory, Compiled_Regex}.

start_timer(Time) ->
	erlang:send_after(Time, self(), {next_run}).
%% --------------------------------------------------------------------
%%
%% --------------------------------------------------------------------
send_to_processing([]) ->
	ok;
send_to_processing([File|Files]) ->
	error_logger:info_msg("proccessing ~p~n", [Files]),
	wurfler_importer:import_wurfl(File),
	send_to_processing(Files).

rename_wurfl(File) ->
	file:rename(File).
%% --------------------------------------------------------------------
%%% create new poll time
%% --------------------------------------------------------------------
new_poll_time(Date, Time) ->
	calendar:datetime_to_gregorian_seconds({Date, Time}).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------