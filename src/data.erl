%% Author: chr
%% Created: May 10, 2012
%% Description: TODO: Add description to data
-module(data).

-define(APPLICATION_NAME, "application_name").
-define(KEY, "key").
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([application_name/1, key/1, serialize/1, deserialize/1, create/3]).

application_name(Data) ->
	proplists:get_value(?APPLICATION_NAME, Data).

key(Data) ->
	proplists:get_value(?KEY, Data).

serialize(Data) -> Data.

deserialize(Data) -> Data.

create(Application_name, Key, Data_list) when is_list(Data_list) ->
	[{?KEY, Key},{?APPLICATION_NAME, Application_name} | Data_list].
