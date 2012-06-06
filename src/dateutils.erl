-module(dateutils).

-export([format/1]).

format({{Year,Month,Day},{Hour,Min,Sec}} = _DateTime) ->
	D = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]),
	lists:flatten(D).