%%%-------------------------------------------------------------------
%%% @author clg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 五月 2020 11:48
%%%-------------------------------------------------------------------
-module(kcp_handle_gateway).
-author("clg").
-include("kcp_misc.hrl").
-include("kcp.hrl").
-include("proto/gateway.hrl").
-include("global.hrl").
%% API
-export([
    apply_kcp/1,
    free_kcp/1]).

-export([
    i/1,
    init_server/0,
    stop_server/0
]).

-export([
    pre_init/1,
    post_init/1,
    pre_terminate/1,
    post_terminate/1,

    do_handle/1,
    handle_msg/2,
    post_send_msg/0,

    check_pack/2,
    safe_pack/2,
    safe_unpack/1,
    worker_pid/1
]).

%% gateway_tcp_client 调用
-export([
    maybe_kcp_active/1,
    maybe_stop_kcp/0,
    maybe_stop_kcp/1,
    kcp_active_pid/0
]).
-export([
    call/1
]).
-define(KCP_MANAGER, ?KCP_MANAGER_SERVER).

i(Type) ->
    kcp_manager:i(?KCP_MANAGER, Type).

call(Msg) ->
    kcp_manager:call(?KCP_MANAGER, Msg).

apply_kcp(AgentPID) ->
    case kcp_manager:apply_conv(?KCP_MANAGER, AgentPID) of
        {ok, Conv, Key} ->
            Num = common_config:get_kcp_port_num(),
            Nth = erlang:phash2(AgentPID, Num) + 1,
            KcpPort = common_config:get_kcp_port(Nth),
            {ok, KcpPort, Conv, Key};
        {error, full} ->
            {error, full}
    end.
free_kcp(Conv) ->
    kcp_manager:close_conv(?KCP_MANAGER, Conv).

init_server() ->
    KcpPorts = common_config:get_kcp_ports(),
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
        || Port <- KcpPorts]).
%% 在哪里启动，就在哪里关闭
stop_server() ->
    KcpPorts = common_config:get_kcp_ports(),
    [ok = kcp_server:stop(Port) || Port <- KcpPorts],
    kcp_manager:stop(?KCP_MANAGER),
    ok.

%%%% ============== worker 进程 callback ===================
pre_init(Conv) ->
    Res = kcp_manager:reg_worker(?KCP_MANAGER, Conv, erlang:self()),
    ?IF(Res=:=ok, ok, erlang:throw(Res)),
    Res.

post_init(_Conv) ->
    mtime:reg(mnode, [1000]),
    set_last_hb_time(mtime:now()).

pre_terminate(_Conv) ->
    ok.

post_terminate(Conv) ->
    catch kcp_manager:close_conv(?KCP_MANAGER, Conv), %% 确保异常关闭也能清理干净数据
    catch mtime:dereg(gate, [1000]).

do_handle({loop_sec, Now}) ->
    mtime:now_cached(Now),
    do_loop_sec();
do_handle(Info) ->
    ?INFO_MSG("~p unknown msg: ~p", [?MODULE, Info]),
    ok.

handle_msg(Conv, Binary) ->
    do_handle_msg(Conv, Binary, get_syn_state()),
    Now = mtime:now(),
    set_last_hb_time(Now),
    Now.

do_handle_msg(_Conv, ?GKCP_PACKET_HEARTBEAT, _State) ->
    kcp_worker:do_send_msg(?GKCP_PACKET_HEARTBEAT);
do_handle_msg(Conv, ?GKCP_PACKET_SYN_SENT, undefined) ->
    %% 收到 SYN_SENT 包，发送SYN_RECV 包，进入 SYN_RECV 状态
    set_syn_state(?GKCP_STATE_SYN_RECV),
    kcp_worker:do_send_msg(?GKCP_PACKET_SYN_RECV),
    case kcp_manager:my_agent_key(?KCP_MANAGER, Conv) of
        {_AgentPID, _Key} ->
            ok;
        undefined ->
            kcp_misc:close(closed)
    end;
do_handle_msg(Conv, ?GKCP_PACKET_ESTABLISHED, ?GKCP_STATE_SYN_RECV) ->
    %% 收到 ESTABLISHED 包，进入 ESTABLISHED 状态
    set_syn_state(?GKCP_STATE_ESTABLISHED),
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

post_send_msg() ->
    ok.

check_pack(_Conv, ?KCP_SEG(_Conv1, _Cmd, Frg, _Wnd, _Ts, _Sn, _Una, _Len, _Data, _Left) = Pack) when Frg > ?MAX_PACK_FRG ->
    ?ERROR_MSG("Recv Too Large Kcp Pack：(Frg:~w)~w", [Frg,Pack]),
    kcp_misc:close(badpack),
    false;
check_pack(_Conv, _Pack) ->
    true.

safe_pack(Conv, Binary) ->
    case kcp_manager:my_agent_key(?KCP_MANAGER, Conv) of
        {_AgentPID, Key} ->
            {ok, ?PACK_KCP_SEG(Key, Binary)};
        undefined ->
            {error, closed}
    end.

safe_unpack(?KC_KCP_SEG(Key, Conv) = InetRecvData) ->
    case kcp_manager:my_agent_key(?KCP_MANAGER, Conv) of
        {_AgentPID, Key} ->
            ?PACK_KCP_SEG(Data) = InetRecvData,
            {ok, Conv, Data};
        _ -> {error, badkey}
    end;
safe_unpack(_InetRecvData) ->
    {error, badpack}.

worker_pid(Conv) ->
    kcp_manager:worker_pid(?KCP_MANAGER, Conv).

do_loop_sec() ->
    maybe_check_hb(),
    ok.

maybe_check_hb() ->
    case kcp_data:is_closing() of
        true -> ignore;
        _ ->
            do_check_heartbeat()
    end.

do_check_heartbeat() ->
    LastTime = get_last_hb_time(),
    Now = mtime:now(),
    case Now - LastTime >= ?GKCP_INTERVAL_TIMEOUT of
        true ->
            kcp_misc:close(hearbeat_timeout);
        false ->
            ok
    end.

get_last_hb_time() ->
    erlang:get(last_hb_time).
set_last_hb_time(T) ->
    erlang:put(last_hb_time, T).

%%  -------------------------------------------------
%% 以下为 gateway_tcp_client 调用
maybe_kcp_active(KcpPid) ->
    case get_kcp_pid() of
        undefined ->
            Ref = erlang:monitor(process, KcpPid),
            set_kcp_pid({KcpPid, Ref}),
            ok;
        {KcpPid, _Ref} ->
            ok;
        _Err ->
            ?ERROR_MSG("error:~w", [{KcpPid, _Err}]),
            ok
    end.

maybe_stop_kcp() ->
    case get_kcp_pid() of
        {KcpPid, Ref} ->
            catch erlang:demonitor(Ref, [flush]),
            erase_kcp_pid(),
            kcp_misc:close(KcpPid, normal),
            gateway_tcp_client:send(#m_kcp_close_toc{}),
            ok;
        _ ->
            ok
    end.

maybe_stop_kcp(KcpPid) ->
    case get_kcp_pid() of
        {KcpPid, Ref} ->
            catch erlang:demonitor(Ref, [flush]),
            erase_kcp_pid(),
            %% kcp_misc:close(KcpPid, normal),
            gateway_tcp_client:send(#m_kcp_close_toc{}),
            true;
        _ ->
            false
    end.

kcp_active_pid() ->
    case get_kcp_pid() of
        {Pid, _} -> Pid;
        _ -> undefined
    end.


get_kcp_pid() ->
    erlang:get(kcp_pid).
set_kcp_pid(KcpPid) ->
    erlang:put(kcp_pid, KcpPid).
erase_kcp_pid() ->
    erlang:erase(kcp_pid).

get_syn_state() ->
    erlang:get(syn_state).
set_syn_state(SynState) ->
    erlang:put(syn_state, SynState).