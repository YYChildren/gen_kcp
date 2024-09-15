%%%-------------------------------------------------------------------
%%% @author yangchaojun
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 五月 2020 11:48
%%%-------------------------------------------------------------------
-module(kcp_handle_robot).
-author("yangchaojun").
-behaviour(behaviour_kcp).

-include("kcp_misc.hrl").
-include("kcp.hrl").
-include("proto/gateway.hrl").

%% API
-export([
    init_server/0,
    stop_server/0,
    start_worker/3,
    add_conv/3,
    active_service/1
]).

-export([
    kcp_pre_init/1,
    kcp_post_init/1,
    kcp_pre_terminate/1,
    kcp_post_terminate/1,

    kcp_do_handle/1,
    kcp_handle_msg/2,
    kcp_post_send_msg/0,

    kcp_check_pack/2,
    kcp_pack/2,
    kcp_unpack/1,
    kcp_worker_pid/1
]).

%% robot_client 调用
-export([
    maybe_kcp_init/1,
    maybe_kcp_active/1,
    maybe_stop_kcp/0,
    after_stop_kcp/1,
    kcp_active_pid/0
]).

-define(KCP_MANAGER, ?KCP_MANAGER_CLIENT).

init_server() ->
    Ports = robot_config:get_kcp_ports(),
    lists:foreach(
        fun ({Msg, Thunk}) ->
                io:format("starting ~-24s ...", [Msg]),
                Thunk(),
                io:format("done~n");
            ({Msg, M, F, A}) ->
                io:format("starting ~-20s ...", [Msg]),
                apply(M, F, A),
                io:format("done~n")
        end,
        [
            {"Kcp Manager", fun() ->
                {ok, _} = kcp_manager:start(?KCP_MANAGER)
            end}
        ] ++ [
            {lists:concat(["Kcp Server ", Port]), fun() ->
                {ok, _} = kcp_server:start(Port, ?MODULE)
            end}
        || Port <- Ports]).
%% 在哪里启动，就在哪里关闭
stop_server() ->
    KcpPorts = robot_config:get_kcp_ports(),
    [ok = kcp_server:stop(Port) || Port <- KcpPorts],
    kcp_manager:stop(?KCP_MANAGER),
    ok.

start_worker(AgentPID, Conv, PeerAddress) ->
    Num = robot_config:get_kcp_port_num(),
    AgentPID = erlang:self(),
    Nth = erlang:phash2(AgentPID, Num) + 1,
    KcpPort = robot_config:get_kcp_port(Nth),
    PName = kcp_server:pname(KcpPort),
    kcp_server:start_worker(PName, Conv, PeerAddress).

active_service(KcpPid) ->
    erlang:send(KcpPid, active_service),
    ok.
add_conv(AgentPID, Conv, Key) ->
    ok = kcp_manager:put_conv(?KCP_MANAGER, AgentPID, Conv, Key),
    ok.

kcp_pre_init(Conv) ->
    Res = kcp_manager:reg_worker(?KCP_MANAGER, Conv, erlang:self()),
    Res.

kcp_post_init(_Conv) ->
    ok.

kcp_pre_terminate(_Conv) ->
    ok.

kcp_post_terminate(Conv) ->
    catch kcp_manager:close_conv(?KCP_MANAGER, Conv). %% 确保异常关闭也能清理干净数据

kcp_do_handle(loop_sec) ->
    erlang:send_after(1000, erlang:self(), loop_sec),
    Now = mtime:now_os(),
    mtime:now_cached(Now),
    do_loop_sec();
kcp_do_handle(active_service) ->
    %% 发送 SYN_SENT 包，进入 SYN_SENT 状态
    kcp_worker:do_send_msg(?GKCP_PACKET_SYN_SENT),
    kcp_data_custom:set_syn_state(?GKCP_STATE_SYN_SENT),
    set_last_hb_time(mtime:now()),
    erlang:send_after(1000, erlang:self(), loop_sec),
    ok;
kcp_do_handle(Info) ->
    ?INFO_MSG("~p unknown msg: ~p", [?MODULE, Info]),
    ok.

kcp_handle_msg(Conv, Binary) ->
    do_handle_msg(Conv, Binary, kcp_data_custom:get_syn_state()),
    Now = mtime:now(),
    set_last_hb_time(Now),
    Now.

do_handle_msg(_Conv, ?GKCP_PACKET_HEARTBEAT, _State) ->
    ok;
do_handle_msg(Conv, ?GKCP_PACKET_SYN_RECV, ?GKCP_STATE_SYN_SENT) ->
    %% 发送 SYN_RECV 包，发送 ESTABLISHED 包，进入 ESTABLISHED 状态
    kcp_data_custom:set_syn_state(?GKCP_STATE_ESTABLISHED),
    kcp_worker:do_send_msg(?GKCP_PACKET_ESTABLISHED),
    case kcp_manager:my_agent_key(?KCP_MANAGER, Conv) of
        {AgentPID, _Key} ->
            erlang:send(AgentPID, {kcp_active, erlang:self()});
        undefined ->
            kcp_misc:close(closed)
    end;
do_handle_msg(Conv, Binary, ?GKCP_STATE_ESTABLISHED) ->
    case kcp_manager:my_agent_key(?KCP_MANAGER, Conv) of
        {AgentPID, _Key} -> erlang:send(AgentPID, {kcp_recv, erlang:self(), Binary});
        undefined -> kcp_misc:close(closed)
    end;
do_handle_msg(Conv, Binary, State) ->
    ?WARNING_MSG("在~w状态时收到不合法数据：~w <- ~w", [State, Conv, Binary]).

kcp_post_send_msg() ->
    set_send_hb_time(mtime:now()).

kcp_check_pack(_Conv, ?KCP_SEG(_Conv1, _Cmd, Frg, _Wnd, _Ts, _Sn, _Una, _Len, _Data, _Left) = Pack) when Frg > ?MAX_PACK_FRG ->
    ?ERROR_MSG("Recv Too Large Kcp Pack：(Frg:~w)~w", [Frg,Pack]),
    kcp_misc:close(badpack),
    false;
kcp_check_pack(_Conv, _Pack) ->
    true.

kcp_pack(Conv, Binary) ->
    case kcp_manager:my_agent_key(?KCP_MANAGER, Conv) of
        {_AgentPID, Key} ->
            {ok, ?PACK_KCP_SEG(Key, Binary)};
        undefined ->
            {error, closed}
    end.

kcp_unpack(?KC_KCP_SEG(Key, Conv) = InetRecvData) ->
    case kcp_manager:my_agent_key(?KCP_MANAGER, Conv) of
        {_AgentPID, Key} ->
            ?PACK_KCP_SEG(Data) = InetRecvData,
            {ok, Conv, Data};
        _ -> {error, badkey}
    end;
kcp_unpack(_InetRecvData) ->
    {error, badpack}.

kcp_worker_pid(Conv) ->
    kcp_manager:worker_pid(?KCP_MANAGER, Conv).

do_loop_sec() ->
    maybe_send_hb(),
    maybe_check_hb(),
    ok.

maybe_send_hb() ->
    LastSend = get_send_hb_time(),
    Now = mtime:now(),
    case Now - LastSend >= ?GKCP_SEND_INTERVAL of
        true ->
            kcp_worker:do_send_msg(?GKCP_PACKET_HEARTBEAT);
        false -> ignore
    end.

maybe_check_hb() ->
    case kcp_data:is_closing() of
        true -> ignore;
        _ ->
            do_check_heartbeat()
    end.


do_check_heartbeat() ->
    LastTime = get_last_hb_time(),
    Now = mtime:now(),
    case kcp_data_custom:get_syn_state() of
        ?GKCP_STATE_ESTABLISHED ->
            case Now - LastTime >= ?GKCP_INTERVAL_TIMEOUT of
                true ->
                    kcp_misc:close(hearbeat_timeout);
                false ->
                    ok
            end;
        _ ->
            case Now - LastTime >= ?GKCP_CONNECT_TIMEOUT of
                true ->
                    kcp_misc:close(connect_timeout);
                false ->
                    ok
            end
    end.

get_send_hb_time() ->
    erlang:get(send_hb_time).
set_send_hb_time(T) ->
    erlang:put(send_hb_time, T).

get_last_hb_time() ->
    erlang:get(last_hb_time).
set_last_hb_time(T) ->
    erlang:put(last_hb_time, T).

%%  -------------------------------------------------
%% 以下为robot_client 调用

maybe_kcp_init(KcpPid) ->
    kcp_data_custom:maybe_kcp_init(KcpPid).

maybe_kcp_active(KcpPid) ->
    kcp_data_custom:maybe_kcp_active(KcpPid).

maybe_stop_kcp() ->
    case kcp_data_custom:get_kcp_pid() of
        {_KcpPid, ?GKCP_STATE_GATE_CLOSED} ->
            true;
        {KcpPid, _} ->
            kcp_data_custom:maybe_set_kcp(KcpPid, ?GKCP_STATE_GATE_CLOSED),
            kcp_misc:close(KcpPid, normal),
            robot_client:tos(#m_kcp_close_tos{}),
            true;
        _ ->
            false
    end.

after_stop_kcp(undefined) ->
    false;
after_stop_kcp(KcpPid) ->
    case kcp_data_custom:get_kcp_pid() of
        {KcpPid, ?GKCP_STATE_GATE_CLOSED} ->
            kcp_data_custom:erase_kcp_pid(),
            true;
        {KcpPid, _} ->
            robot_client:tos(#m_kcp_close_tos{}),
            kcp_data_custom:erase_kcp_pid(),
            true;
        _ -> false
    end.

kcp_active_pid() ->
    kcp_data_custom:kcp_active_pid().