%%%-------------------------------------------------------------------
%% @doc amaze_client top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(amaze_client_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
start_child(Transport) ->
    case application:get_all_env(amaze_client) of
        []->
            {error, no_env};
        Envs->
            TransportArgs =
                lists:filtermap(
                    fun({Transport0,Args}) when Transport0==Transport->
                        {true,Args#{transport_handle=>Transport}};
                        (_)-> false
                    end, Envs ),

            lists:foreach(
                fun(Arg)->
                    Module = amaze_client_mock,
                    ChildSpec = #{
                        id => Module,
                        start => {Module, start_link, [Arg]},
                        restart => permanent,
                        shutdown => 5000,
                        type => worker,
                        modules => [Module]
                    },
                    supervisor:start_child(?SERVER, ChildSpec)
                end, TransportArgs)
    end.
