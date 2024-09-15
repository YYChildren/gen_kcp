%%%-------------------------------------------------------------------
%%% @author yangchaojun
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 五月 2020 11:48
%%%-------------------------------------------------------------------
-module(mnode_gateway_kcp_handle).
-behaviour(behaviour_kcp).
-author("yangchaojun").
-include("global.hrl").
-include("kcp_misc.hrl").
-include("kcp.hrl").

%% API
-export([
    init_server/0,
    stop_server/0
]).

-export([
    kcp_pre_init/1,
    kcp_post_init/1,
    kcp_pre_terminate/1,
    kcp_post_terminate/1,

    kcp_do_handle/1,
    kcp_post_send_msg/0,
    kcp_handle_msg/2,
    post_send_msg/0,

    kcp_check_pack/2,
    kcp_pack/2,
    kcp_unpack/1,
    kcp_worker_pid/1
]).

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
            {lists:concat(["Kcp Server ", Port]), fun() ->
                {ok, _} = kcp_server:start(Port, ?MODULE)
            end}
        || Port <- KcpPorts]).
%% 在哪里启动，就在哪里关闭
stop_server() ->
    KcpPorts = common_config:get_kcp_ports(),
    [ok = kcp_server:stop(Port) || Port <- KcpPorts],
    ok.

%%%% ============== worker 进程 callback ===================
kcp_pre_init(Conv) ->
    case mnode_gateway_manager:agent_proc(Conv) of
        undefined ->
            erlang:throw({error, no_agent});
        AgentProc ->
            case mnode_gateway_worker:reg_kcp_proc(AgentProc, Conv, erlang:self()) of
                ok -> ok;
                Error -> erlang:throw(Error)
            end
    end.


kcp_post_init(_Conv) ->
    mtime:reg(mnode, [1000]),
    set_last_hb_time(mtime:now()).

kcp_pre_terminate(_Conv) ->
    ok.

kcp_post_terminate(Conv) ->
    catch mnode_gateway_manager:close_conv(Conv), %% 确保异常关闭也能清理干净数据
    catch mtime:dereg(gate, [1000]).

kcp_do_handle({loop_sec, Now}) ->
    mtime:now_cached(Now),
    do_loop_sec();
kcp_do_handle(Info) ->
    ?INFO_MSG("~p unknown msg: ~p", [?MODULE, Info]),
    ok.

kcp_post_send_msg() ->
    ok.

kcp_handle_msg(Conv, Binary) ->
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
    case mnode_gateway_manager:kcp_key(Conv) of
        undefined ->
            kcp_misc:close(closed);
        _ ->
            ok
    end;
do_handle_msg(Conv, ?GKCP_PACKET_ESTABLISHED, ?GKCP_STATE_SYN_RECV) ->
    %% 收到 ESTABLISHED 包，进入 ESTABLISHED 状态
    set_syn_state(?GKCP_STATE_ESTABLISHED),
    case mnode_gateway_manager:agent_proc(Conv) of
        undefined ->
            kcp_misc:close(closed);
        AgentProc ->
            case mnode_gateway_worker:activate_kcp_proc(AgentProc, Conv, erlang:self()) of
                ok -> ok;
                _ -> kcp_misc:close(closed)
            end
    end;
do_handle_msg(Conv, Binary, ?GKCP_STATE_ESTABLISHED) ->
    %% 原封不动地广播所有客户端
    kcp_worker:do_send_msg(?GKCP_PACKET_HEARTBEAT),
    ConvList = mnode_gateway_manager:get_conv2con_list(Conv),
    case is_sync_active(ConvList) of
        true ->
            do_send_pending(ConvList),
            do_send_pack(Binary, ConvList);
        false ->
            add_pending_pack(Binary)
    end;
do_handle_msg(Conv, Binary, State) ->
    ?WARNING_MSG("在~w状态时收到不合法数据：~w <- ~w", [State, Conv, Binary]).

maybe_send_pending() ->
    Conv = kcp_data:get_conv(),
    ConvList =  mnode_gateway_manager:get_conv2con_list(Conv),
    case is_sync_active(ConvList) of
        true -> do_send_pending(ConvList);
        false -> ignore
    end.

%% @doc 发送一些 pending包
do_send_pending(ConvList) ->
    {_Len, PackList} = get_pending_pack_list(),
    [begin
       KcpProc = mnode_gateway_manager:kcp_proc(ToConv),
       kcp_misc:send_msg(KcpProc, Binary)
    end || ToConv <- ConvList, Binary <- lists:reverse(PackList)],
    erase_pending_pack_list().

%% @doc 发送数据包
do_send_pack(Binary, ConvList) ->
    [begin
        KcpProc = mnode_gateway_manager:kcp_proc(ToConv),
        kcp_misc:send_msg(KcpProc, Binary)
    end || ToConv <- ConvList],
    ok.

is_sync_active([]) ->
    false;
is_sync_active(ConvList) ->
    lists:all(fun mnode_gateway_manager:is_active/1, ConvList).

post_send_msg() ->
    ok.

kcp_check_pack(_Conv, ?KCP_SEG(_Conv1, _Cmd, Frg, _Wnd, _Ts, _Sn, _Una, _Len, _Data, _Left) = Pack) when Frg > ?MAX_PACK_FRG ->
    ?ERROR_MSG("Recv Too Large Kcp Pack：(Frg:~w)~w", [Frg,Pack]),
    kcp_misc:close(badpack),
    false;
kcp_check_pack(_Conv, _Pack) ->
    true.

kcp_pack(Conv, Binary) ->
    case mnode_gateway_manager:kcp_key(Conv) of
        undefined ->
            {error, closed};
        Key ->
            {ok, ?PACK_KCP_SEG(Key, Binary)}
    end.

kcp_unpack(?KC_KCP_SEG(Key, Conv) = InetRecvData) ->
    case mnode_gateway_manager:kcp_key(Conv) of
        Key ->
            ?PACK_KCP_SEG(Data) = InetRecvData,
            {ok, Conv, Data};
        _ ->
            {error, badkey}
    end;
kcp_unpack(_InetRecvData) ->
    {error, badpack}.

kcp_worker_pid(Conv) ->
    mnode_gateway_manager:kcp_proc(Conv).

do_loop_sec() ->
    maybe_send_pending(),
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

get_syn_state() ->
    erlang:get(syn_state).
set_syn_state(SynState) ->
    erlang:put(syn_state, SynState).

add_pending_pack(Binary) ->
    {Len, PackList} = get_pending_pack_list(),
    set_pending_pack_list({Len + 1, [Binary | PackList]}).

get_pending_pack_list() ->
    case erlang:get(pending_pack_list) of
        undefined -> {0, []};  %% 先预留长度，方便后续做网关保护
        LPack -> LPack
    end.
set_pending_pack_list(undefined) ->
    erase_pending_pack_list();
set_pending_pack_list({0, []}) ->
    erase_pending_pack_list();
set_pending_pack_list(Pack) ->
    erlang:put(pending_pack_list, Pack).
erase_pending_pack_list() ->
    erlang:erase(pending_pack_list).
