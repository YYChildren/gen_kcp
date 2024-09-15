%%%-------------------------------------------------------------------
%%% @author yangchaojun
%%% @copyright (C) 2020, <www.mingchao.com>
%%% @doc
%%%
%%% @end
%%% Created : 01. 六月 2020 18:37
%%%-------------------------------------------------------------------
-module(kcp_data_custom).
-author("yangchaojun").
-include("kcp_misc.hrl").


%% API
%% KCP 进程
-export([
    get_syn_state/0,
    set_syn_state/1
]).
%% gate 进程
-export([
    maybe_kcp_init/1,
    maybe_kcp_active/1,
    maybe_set_kcp/2,
    kcp_active_pid/0,
    get_kcp_pid/0,
    set_kcp_pid/1,
    erase_kcp_pid/0
]).

%% kcp_manager 使用
-export([
    get_manager_name/0,
    set_manager_name/1
]).


get_syn_state() ->
    erlang:get(syn_state).
set_syn_state(SynState) ->
    erlang:put(syn_state, SynState).

%%  ---------------------------------------------------------------

maybe_kcp_init(KcpPid) ->
    maybe_set_kcp(KcpPid, ?GKCP_STATE_GATE_INIT).

maybe_kcp_active(KcpPid) ->
    maybe_set_kcp(KcpPid, ?GKCP_STATE_GATE_ACTIVE).


maybe_set_kcp(KcpPid, State) ->
    case get_kcp_pid() of
        {KcpPid, State} ->
            true;
        undefined when State =:= ?GKCP_STATE_GATE_INIT ->
            erlang:monitor(process, KcpPid),
            set_kcp_pid({KcpPid, State}),
            true;
        {KcpPid, ?GKCP_STATE_GATE_INIT} when State =:= ?GKCP_STATE_GATE_ACTIVE ->
            set_kcp_pid({KcpPid, State}),
            true;
        {KcpPid, _} when State =:= ?GKCP_STATE_GATE_CLOSED ->
            set_kcp_pid({KcpPid, State}),
            true;
        _ ->
            false
    end.

kcp_active_pid() ->
    case kcp_data_custom:get_kcp_pid() of
        {Pid, ?GKCP_STATE_GATE_ACTIVE} ->
            case erlang:is_process_alive(Pid) of
               true -> Pid;
                _ -> undefined
            end;
        _ -> undefined
    end.

get_kcp_pid() ->
    erlang:get(kcp_pid).
set_kcp_pid(KcpPid) ->
    erlang:put(kcp_pid, KcpPid).
erase_kcp_pid() ->
    erlang:erase(kcp_pid).

get_manager_name() ->
    erlang:get(manager_name).

set_manager_name(Name) ->
    erlang:put(manager_name, Name).