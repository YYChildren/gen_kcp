%%%-------------------------------------------------------------------
%% @doc kcp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(kcp_sup).

-behaviour(supervisor).

-export([start_link/0]).

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
    SupFlags = {one_for_one, 10, 10},
    ChildSpecs = [
        {kcp_throttle, {kcp_throttle, start_link, []}, permanent, infinity, worker, [kcp_throttle]},
        {kcp_server_sup, {kcp_server_sup, start_link, []}, transient, infinity, supervisor, [kcp_server_sup]},
        {kcp_worker_sup, {kcp_worker_sup, start_link, []}, transient, infinity, supervisor, [kcp_worker_sup]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
