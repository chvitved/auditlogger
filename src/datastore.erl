%% Author: chr
%% Created: May 9, 2012
%% Description: TODO: Add description to datastore
-module(datastore).

-define(DEFAULT_BUCKET_PROPERTIES, [{n_val=3}, {r=quorum}, {w=quorum}, {dw=quorum}, {pr=quorum}, {pw=quorum}, 
							{allow_mult, true}, {last_write_wins, false}, {basic_quorum, false}, {notfound_ok, false,}]).

-define(PUT_OPTIONS, [if_none_match]).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API functions
-export([start_link/0, put/4, get/3, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {riak_client}).

%%
%% API Functions
%%
start_link() ->
	gen_server:start_link(?MODULE, [], []).

put(DatastorePid, Bucket, Key, Term) ->
  gen_server:call(DatastorePid, {put, Bucket, Key, Term}).
  
get(DatastorePid, Bucket, Key) ->
  gen_server:call(DatastorePid, {get, Bucket, Key}).

close(DatastorePid) -> 
	gen_server:cast(DatastorePid, stop).

%%set_bucket_default_properties(DatastorePid, Bucket) ->
%%	gen_server:call(DatastorePid, {set_bucket_properties, Bucket, ?DEFAULT_BUCKET_PROPERTIES}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087), %% for now this is started blocking
  {ok, #state{riak_client=Pid}}.

handle_call({put, Bucket, Key, Term}, _From, #state{riak_client=Riak_client}=State) ->
	Result = do_put(Bucket, Key, Term, Riak_client),
    {reply, Result, State};

handle_call({get, Bucket, Key}, _From, #state{riak_client=Riak_client}=State) ->
	{ok, Value} = do_get(Bucket, Key, Riak_client),
	{reply, {ok, Value}, State}.

%handle_call({set_bucket_properties, Bucket, Properties}, _From, #state{riak_client=Riak_client}=State) when is_list(Properties) ->
%	riakc_pb_socket:set_bucket(Riak_client, Bucket, Properties),
%	{reply, ok, State}.

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

do_put(Bucket, Key, Term, Riak_client) ->
	Object = riakc_obj:new(list_to_binary(Bucket), list_to_binary(Key), term_to_binary(Term), <<"application/x-erlang-term">>),
	riakc_pb_socket:put(Riak_client, Object, ?PUT_OPTIONS).

do_get(Bucket, Key, Riak_client) ->
	case riakc_pb_socket:get(Riak_client, list_to_binary(Bucket), list_to_binary(Key)) of 
		{ok, Object} ->
			Value = binary_to_term(riakc_obj:get_value(Object)), 
			{ok, Value};
		{error, notfound} ->  {ok, notfound};
		Error -> Error
	end.

	

%%%===================================================================
%%% tests
%%%===================================================================
-ifdef(TEST).
	
%%% TODO, we should support replays - but maybe detect them like this and then inform the client it was regarded as a replay
put_same_key_twice_test() ->
	{ok, DatastorePid} = start_link(),
	Bucket = "test",
	Key = "test" ++ dateutils:format(erlang:localtime()),
	Value = "value",
	put(DatastorePid, Bucket, Key, Value),
	?assertEqual({error,<<"match_found">>}, put(DatastorePid, Bucket, Key, Value)).

-endif.