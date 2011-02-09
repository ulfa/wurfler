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
-module(wurfler_string_metrics).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("../include/wurfler.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([levenshtein/2]).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%%------------------------------------------------------------------------------
%% @spec levenshtein(StringA, StringB) -> integer()
%%		StringA = StringB = string()
%% @doc Calculates the Levenshtein distance between two strings
%% @end
%%------------------------------------------------------------------------------
levenshtein(Samestring, Samestring) -> 0;
levenshtein(String, []) -> length(String);
levenshtein([], String) -> length(String);
levenshtein(Source, Target) ->
	levenshtein_rec(Source, Target, lists:seq(0, length(Target)), 1).

%% Recurses over every character in the source string and calculates a list of distances
levenshtein_rec([SrcHead|SrcTail], Target, DistList, Step) ->
	levenshtein_rec(SrcTail, Target, levenshtein_distlist(Target, DistList, SrcHead, [Step], Step), Step + 1);
levenshtein_rec([], _, DistList, _) ->
	lists:last(DistList).

%% Generates a distance list with distance values for every character in the target string
levenshtein_distlist([TargetHead|TargetTail], [DLH|DLT], SourceChar, NewDistList, LastDist) when length(DLT) > 0 ->
	Min = lists:min([LastDist + 1, hd(DLT) + 1, DLH + dif(TargetHead, SourceChar)]),
	levenshtein_distlist(TargetTail, DLT, SourceChar, NewDistList ++ [Min], Min);
levenshtein_distlist([], _, _, NewDistList, _) ->
	NewDistList.

% Calculates the difference between two characters or other values
dif(C, C) -> 0;
dif(_, _) -> 1.

%% --------------------------------------------------------------------
%%% Test functions
%% -------------------------------------------------------------------
levenshtein_test_() ->
	{setup, 
	 	fun() -> setup() end,
	 	fun(_) ->
			[
			 ?_assertEqual(2, levenshtein("Aloha!", "Alhoa!")),
			 ?_assertEqual(39,levenshtein(string:to_lower("Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_1 like MacM OS X; de-de) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8B117 Safari/6531.22.7"),
				string:to_lower("Mozilla/5.0 (iPhone; U; CPU like Mac OS X; en) AppleWebKit/420+ (KHTML, like Gecko) Version/3.0 Mobile/1A538a Safari/419.3")))
			 ]
	 	end
	 }.	

setup() ->
	mnesia:clear_table(devicesTbl),
	mnesia:clear_table(brand_index),
	mnesia:load_textfile("data/daten.txt").

levenshtein_DB_test_() ->
	Keys = wurfler_db:get_all_keys(devicesTbl),
	A="Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_1 like MacM OS X; de-de) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8B117 Safari/6531.22.7",
	parmap(A, Keys).
	%get_devices(Keys, []).
		
get_devices([], Acc)->
	Acc;
get_devices([Key|Keys], Acc) ->
	[#device{user_agent=UA}] = wurfler_db:find_record_by_id(devicesTbl, Key),
	A="Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_1 like MacM OS X; de-de) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8B117 Safari/6531.22.7",
	%%Diff = levenshtein(UA,A), %%28245692 on a core2duo macbookPro
	erlang:spawn(fun() -> wurfler_string_metrics:levenshtein(UA, A)  end),
	%%10121671 on a core2duo macbookPro
	%%5009021 on my amd box
	Diff = levenshtein(string:to_lower(string:substr(A, 1, erlang:length(A))), string:to_lower(UA)),
	io:format("~p, ~p, ~p ~n", [Diff, erlang:length(UA), UA]),
	get_devices(Keys, Acc).

parmap(UA ,Keys) ->
    Parent = self(),
    [receive 
		 {Pid, Result} -> Result 
	 end || Pid <- [spawn(fun() -> Parent ! {self(), wurfler_string_metrics:levenshtein(UA, get_a_ua(Key))} end) || Key <- Keys]
	].

get_a_ua(Key) ->
	[#device{user_agent=UA}] = wurfler_db:find_record_by_id(devicesTbl, Key),
	UA.