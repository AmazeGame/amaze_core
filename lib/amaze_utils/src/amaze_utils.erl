-module(amaze_utils).

-export([sprintf/2]).

sprintf(Format,Args)->
	lists:flatten(io_lib:format(Format,Args)).