%%%-------------------------------------------------------------------
%% @doc amaze_core top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(amaze_core_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.
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
-spec init([])
        -> {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },

    EtsServer = #{
        id=> az_ets_server,
        start=> {az_ets_server,start_link,[]},
        restart => permanent,
        shutdown => brutal_kill,
        type=> worker
    },

    ChildSpecs = [EtsServer],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
