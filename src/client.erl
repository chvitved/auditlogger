%% Author: chr
%% Created: May 10, 2012
%% Description: TODO: Add description to client
-module(client).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%
start() ->
	Seq = lists:seq(1, 1000),
	[save() || _S <- Seq].
	

%%
%% Local Functions
%%

save() ->
	Data = gen_data_element(),
	Key = data:key(Data),
	Application = data:application_name(Data),
	auditlogger:save(Data),
	{ok, Res} = auditlogger:get(Application, Key),
	io:format("Data is ~p~n", [data:serialize(Res)]).

gen_data_element() ->
	Key = key_gen:gen_key(), 
	Person = gen_person(),
	Doctor = gen_string(8),
	Service_name = gen_string(10 + random:uniform(10)),
	Long_string = gen_string(500),
	Data = [{"person", Person},{"doctor", Doctor},{"service", Service_name},{"more_data",Long_string}],
	data:create("myapplication", Key, Data).
	

gen_person() ->
	R = random:uniform(5000000),
	Res = io_lib:format("~10.10.0B",[R]),
	lists:flatten(Res).

gen_string(Length) -> gen_string(Length, []).

gen_string(0, Acc) -> Acc;

%ascii chars between 97-122	
gen_string(Length, Acc) ->
	R = random:uniform(122-97) + 97,
	gen_string(Length -1, [R | Acc]).
	