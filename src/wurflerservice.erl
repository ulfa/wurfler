%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
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
%% @spec ensure_started(App::atom()) -> ok
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
	WurflerConfig={wurfler_config,
				{wurfler_config, start_link, []},
              	permanent,
              	10000,
              	worker,
              	[wurfler_config]},	
	Wurfler={wurfler,
				{wurfler, start_link, []},
              	permanent,
              	10000,
              	worker,
              	[wurfler]},
	WurflerSearch={wurfler_search,
				{wurfler_search, start_link, []},
              	permanent,
              	10000,
              	worker,
              	[wurfler_search]},
	WurflerUAParser={wurfler_uaparser,
				{wurfler_uaparser, start_link, []},
              	permanent,
              	10000,
              	worker,
              	[wurfler_uaparser]},	
	WurflerCache={wurfler_cache,
				{wurfler_cache, start_link, []},
              	permanent,
              	10000,
              	worker,
              	[wurfler_cache]},
	WurflerFilePoller={wurfler_file_poller,
				{wurfler_file_poller, start_link, []},
              	permanent,
              	10000,
              	worker,
              	[wurfler_importer]},
	WurflerImporter={wurfler_importer,
				{wurfler_importer, start_link, []},
              	permanent,
              	10000,
              	worker,
              	[wurfler_importer]},
	WurflerUpdate={wurfler_update,
				{wurfler_update, start_link, []},
              	permanent,
              	10000,
              	worker,
              	[wurfler_update]},		
	WurflerPatch={wurfler_patch,
				{wurfler_patch, start_link, []},
              	permanent,
              	10000,
              	worker,
              	[wurfler_patch]},	
	WebConfig = [
                 {ip, Ip},
                 {backlog, 1000},
                 {port, 8000},
                 {log_dir, "logs/weblog"},
                 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
		{ok, {{one_for_one, 3, 10},
		   [
			WurflerConfig,
			Wurfler,
			WurflerSearch,
			WurflerCache,
			WurflerUAParser,
			WurflerFilePoller,
			WurflerImporter,
			WurflerUpdate,
			WurflerPatch,
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
	ensure_started(mnesia),
    ensure_started(webmachine),
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
	application:stop(crypto),
	application:stop(webmachine),
	application:stop(mnesia),
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

