%%%-------------------------------------------------------------------
%% @doc kcp public API
%% @end
%%%-------------------------------------------------------------------

-module(gen_kcp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gen_kcp:load(),
    {ok, Pid} = kcp_sup:start_link(),
    {ok, Pid}.

stop(_State) ->
    gen_kcp:unload(),
    ok.

%% internal functions
