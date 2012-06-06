%% Author: chr
%% Created: Apr 30, 2012
%% Description: TODO: Add description to auditlogger
-module(auditlogger).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API functions

-export([start_link/0, save/1, get/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {datastore}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

save(Data) ->
  gen_server:call(?SERVER, {save, Data}).
  
get(Application, Key) ->
  gen_server:call(?SERVER, {get, Application, Key}).

stop() ->
    gen_server:call(?SERVER, stop).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, Pid} = datastore:start_link(),
  {ok, #state{datastore=Pid}}.

handle_call({save, Data}, _From, #state{datastore=Datastore}=State) ->
	Res = save(Data, Datastore),
    {reply, Res, State};

handle_call({get, Application, Key}, _From, #state{datastore=Datastore}=State) ->
	Res = get(Application, Key, Datastore),
	{reply, Res, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.  


%%%===================================================================
%%% local functions
%%%===================================================================

save(Data, Datastore) -> save(Data, Datastore, 0).

save(_Data, _Datastore, 3, Last_error_term) ->
	{error, Last_error_term};

save(Data, Datastore, RetryNumber, _Last_error_term) ->
	save(Data, Datastore, RetryNumber).

save(Data, Datastore, RetryNumber) ->
	Bucket = data:application_name(Data),
	Key = data:key(Data),
	Serialized_data = data:serialize(Data),
	case datastore:put(Datastore, Bucket, Key, Serialized_data) of 
		ok -> ok;
		{error, ErrorMsg} -> 
			error_logger:warning_msg("got an error ~s, saving the key ~s with value ~p", [ErrorMsg, Key, data:serialize(Data)]),
			save(Data, Datastore, RetryNumber + 1, ErrorMsg)
	end.

get(Application, Key, Datastore) ->
	case datastore:get(Datastore, Application, Key) of
		{ok, notfound} -> {ok, notfound}; 
		{ok, Serialized_data} -> {ok, data:deserialize(Serialized_data)};
		Error -> Error 
	end.
	



%%%===================================================================
%%% tests
%%%===================================================================
-ifdef(TEST).
-record(data, {key, name}).

setup() -> 
	{ok, _} = start_link(),
	Key = key_gen:gen_key(),
	Application_name = "test",
	#data{key=Key, name=Application_name}.

teardown(_Data) -> stop().

get_non_existing_key_test_() ->
     {setup,
      fun setup/0,
	  fun teardown/1,
      fun(Data) -> ?_assertEqual({ok, notfound}, get(Data#data.name, Data#data.key)) end
	 }.

save_and_get_test_() ->
	{setup,
     fun setup/0,
	 fun teardown/1,
     fun(Data) -> ?_test(
			begin
				Data_list = [{"person", "Åge Sørensen"},{"doctor", "Testlæge Pedersen"},{"service", "Hent laboratorieprøver"},{"id",123456}, {"tidspunkt", "17:12:2012 20:04:45"}],
				D = data:create(Data#data.name, Data#data.key, Data_list),
				save(D),
				{ok, Res} = get(Data#data.name, Data#data.key),
				?assertEqual(D, Res)
			end			
		) end
	 }.



-endif.