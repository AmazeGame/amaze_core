%%%-------------------------------------------------------------------
%% @doc amaze_core public API
%% @end
%%%-------------------------------------------------------------------

-module(amaze_core_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(_, _) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    amaze_core_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
    ok.

%% internal functions
