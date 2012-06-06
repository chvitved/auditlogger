%% Author: chr
%% Created: May 25, 2012
-module(application_config).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("auditlogger.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/0, get/1, add/2, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {applications=[]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get(App_name) ->
  gen_server:call(?SERVER, {get, App_name}).

add(App_name, Columns) ->
   gen_server:call(?SERVER, {add, App_name, Columns}).

stop() ->
	gen_server:call(?SERVER, stop).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{applications=[]}}.

handle_call({get, App_name}, _From, State) ->
    {reply, proplists:get_value(App_name, State#state.applications), State};

handle_call({add, App_name, Columns}, _From, State) ->
	New_state = State#state{applications=[{App_name, Columns} | State#state.applications]},
    {reply, ok, New_state};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

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



%%%===================================================================
%%% tests
%%%===================================================================
-ifdef(TEST).

setup() -> 
    {ok,Pid} = application_config:start_link(),
	Pid.
	
teardown(_Pid) -> stop(). 
    
get_test_() ->
     {setup,
      fun setup/0,
	  fun teardown/1,
      fun(_Data) -> ?_test(
			begin
				Test_app_columns = [
					#column{name="person", required=true, index=true},
					#column{name="organisation", required=false, index=false},
 					#column{name="service", required=true, index=true},
					#column{name="comment"}
			 	],
				add("test", Test_app_columns),
				?assertEqual(Test_app_columns, application_config:get("test"))
			end			
		) end
	 }.

-endif.
