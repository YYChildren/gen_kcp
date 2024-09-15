%%%-------------------------------------------------------------------
%% @doc kcp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(kcp_worker_sup).

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
    SupFlags = {simple_one_for_one,10,10},
    ChildSpecs = [{kcp_worker, {kcp_worker, start_link, []}, temporary, infinity, worker, [kcp_worker]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
