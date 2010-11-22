%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(wurflerservice).

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
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
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
	Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Dispatch} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "dispatch.conf"])),
	Wurfler={wurfler,
				{wurfler, start_link, []},
              	permanent,
              	10000,
              	worker,
              	[wurfler]},
	WebConfig = [
                 {ip, Ip},
                 {backlog, 1000},
                 {port, 8000},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
		{ok, {{one_for_one, 3, 10},
		   [
			Wurfler,
			Web
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
	ensure_started(crypto),
    ensure_started(webmachine),
	ensure_started(erlbuild),
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

