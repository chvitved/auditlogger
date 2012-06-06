-module(auditlogger_app).

-behaviour(application).

-export([
	 start/0,
     start/2,
     stop/1
     ]).

% for development
start() -> start(undefined, undefined).

start(_Type, _StartArgs) ->
    auditlogger_sup:start_link().

stop(_State) ->
    ok.