%% Author: chr
%% Created: May 17, 2012
%% Description: TODO: Add description to key_gen
-module(key_gen).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([gen_key/0]).

%%
%% API Functions
%%
gen_key() ->
	Time = calendar:now_to_universal_time(now()),
	Random = random:uniform(1 bsl 64),
	Key = dateutils:format(Time) ++ " @" ++ integer_to_list(Random),
	lists:flatten(Key).


%%
%% Local Functions
%%

