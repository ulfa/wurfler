%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(wurfler_service).

-behaviour(application).
-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([start/0, start/2, stop/0, stop/1, restart/0]).
-export([init/1, start_link/2]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	Wurfler={wurfler,
         	 {code_reloader, start_link, []},
              permanent,
              10000,
              worker,
              [wurfler]},
	{ok, {{one_for_one, 3, 10},
		   [
			Wurfler
			]}}.
%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/0
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start() ->
	application:start(?MODULE).

start_link(_Type, _Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%% --------------------------------------------------------------------
%% Func: stop/0
%% Returns: any
%% --------------------------------------------------------------------
stop() ->
    application:stop(?MODULE).
%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.
%% --------------------------------------------------------------------
%% Func: restart/0
%% Returns: any
%% --------------------------------------------------------------------
restart() ->
    stop(),
    start().

%% ====================================================================
%% Internal functions
%% ====================================================================

