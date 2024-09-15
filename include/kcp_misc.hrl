%%%-------------------------------------------------------------------
%%% @author yangchaojun
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 四月 2020 11:43
%%%-------------------------------------------------------------------
-author("yangchaojun").

-ifndef(KCP_MISC_HRL).
-define(KCP_MISC_HRL, kcp_misc_hrl).

-include("mgee.hrl").

-define(KCP_MTU_DEF, 1400).
-define(KCP_OVERHEAD, 24).

%% default 32
%% in kcptun video: 1024
%% for game,recommend: 32-256
-define(KCP_SNDWND, 128).
-define(KCP_RCVWND, 128).

%% default: ikcp_nodelay(kcp, 0, 100, 0, 0)
%% fastest: ikcp_nodelay(kcp, 1, 20, 2, 1)
%% nodelay: 0:disable(default), 1:enable, 2: fastest
%% interval: internal update timer interval in millisec, default is 100ms
%% resend: 0:disable fast resend(default), 1:enable fast resend 2: resent after 2 repeat ack
%% nc: 0:normal congestion control(default), 1:disable congestion control
-define(KCP_NODELAY, 1).
-define(KCP_INTERVAL, 20).
-define(KCP_RESEND, 2).
-define(KCP_NC, 1).

%% 这里添加自己的key，方便做校验
%% 特别需要注意的是：byte_size(ip_head/20-60) + byte_size(udp_head/8) + byte_size(key) + KCP_MTU_DEF <= 1480(mtu)
%% 以确保IP层不会分片输入到链路层。如果不满足条件，可以将 KCP_MTU_DEF调小。
%% 备注： MTU 一般来说应该是 1500，但是有些家用路由器 ASDL 走的PPPoE 协议，在IP层和数据链路层之间插入 8 字节的包头
%%        或者说云服务商加入一些包头，一般来说不会超过20字节，最终的实际MTU值应该是1480比较合理
-define(KC_KCP_SEG(__KEY, __CONV), <<__KEY:32/little, __CONV:32/little, _/binary>>).
-define(PACK_KCP_SEG(__KEY, __KCP_SEG), <<__KEY:32/little, __KCP_SEG/binary>>).
-define(PACK_KCP_SEG(__KCP_SEG), ?PACK_KCP_SEG(_, __KCP_SEG)).

%% 4096 大小的包在1400mtu下的最大Frg是2
-define(MAX_PACK_FRG, 2).

%% --------------------
%% 定义 kcp_manager 的相关进程和ETS表
-define(KCP_MANAGER_SERVER, kcp_manager_server).
-define(KCP_MANAGER_CLIENT, kcp_manager_client).
-define(ETS_KCP_MANAGER(__NAME), maps:get((__NAME), #{
    ?KCP_MANAGER_SERVER => ets_kcp_manager_server,
    ?KCP_MANAGER_CLIENT => ets_kcp_manager_client
})).

%% KCP 心跳包
-define(GKCP_PACKET_HEARTBEAT, <<255>>).
%% KCP 三次握手的包
-define(GKCP_PACKET_SYN_SENT, <<1>>).
-define(GKCP_PACKET_SYN_RECV, <<2>>).
-define(GKCP_PACKET_ESTABLISHED, <<3>>).

%% KCP 三次握手协议状态
-define(GKCP_STATE_SYN_SENT, 1).
-define(GKCP_STATE_SYN_RECV, 2).
-define(GKCP_STATE_ESTABLISHED, 3).

%% 连接相关时间
-define(GKCP_SEND_INTERVAL, 8).
-define(GKCP_INTERVAL_TIMEOUT, 30).
-define(GKCP_CONNECT_TIMEOUT, 2).

%% 这里直接写死具体的模块调用，原因是静态调用比动态调用更快
%% 需要新增处理模块时，直接写死具体的模块名
-define(DO_KCP_HANDLE(__F),
    case kcp_data:get_handler() of
        kcp_handle_gateway -> kcp_handle_gateway:__F;
        mnode_gateway_kcp_handle -> mnode_gateway_kcp_handle:__F;
        kcp_handle_robot -> kcp_handle_robot:__F
    end
).
-define(KCP_HANDLE(), ?DO_KCP_HANDLE(?FUNCTION_NAME())).
-define(KCP_HANDLE(__1), ?DO_KCP_HANDLE(?FUNCTION_NAME(__1))).
-define(KCP_HANDLE(__1,__2), ?DO_KCP_HANDLE(?FUNCTION_NAME(__1,__2))).
-define(KCP_HANDLE(__1,__2,__3), ?DO_KCP_HANDLE(?FUNCTION_NAME(__1,__2,__3))).
-define(KCP_HANDLE(__1,__2,__3,__4), ?DO_KCP_HANDLE(?FUNCTION_NAME(__1,__2,__3,__4))).

-define(KCP_CALLBACK(__ARGS, __DEFAULT), begin
    __MODULE = kcp_data:get_handler(),
    case erlang:function_exported(__MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY) of
        true -> erlang:apply(__MODULE, ?FUNCTION_NAME, __ARGS);
        false -> __DEFAULT
    end
end).
-define(KCP_CALLBACK(__ARGS), ?KCP_CALLBACK(__ARGS, undefined)).

-define(KCP_EXPR(__EXPR), ?IF(mconf_dyn:find(switching, log_kcp) =:= [true], (__EXPR), ok)).
-define(KCP_TRY_EXPR(__EXPR), ?KCP_EXPR(?TRY_CATCH(__EXPR))).

%% 日志记录
-define(KCP_INFO_MSG(__FMT, __ARG), ?KCP_EXPR(?INFO_MSG(__FMT, __ARG))).
-define(KCP_WARNING_MSG(__FMT, __ARG), ?KCP_EXPR(?WARNING_MSG(__FMT, __ARG))).
-define(KCP_ERROR_MSG(__FMT, __ARG), ?KCP_EXPR(?ERROR_MSG(__FMT, __ARG))).

-record(arg_kcp_worker_start, {spid, sport, socket, peer, conv, handler}).

%%  -------------------我是华丽的分割线---------------------

%% 辅助状态（gate进程使用）
-define(GKCP_STATE_GATE_INIT, 1).
-define(GKCP_STATE_GATE_ACTIVE, 2).
-define(GKCP_STATE_GATE_CLOSED, 3).

%%  -------------------我是华丽的分割线---------------------

%% 辅助状态（kcp_manager使用）
-define(GKCP_CONV_MAX,    16#FFFFFFFF).
-define(GKCP_KEY_MAX,     16#FFFFFFFF).

%%  -------------------我是华丽的分割线---------------------
%% 发送给对端时对Code转换
-define(CODE_KCP_CLOSE_TO_SEND(__CODE), (
    if (__CODE) =:= 0 -> 0;
        erlang:is_integer(__CODE) -> ?PROTO_ERROR_CODE_PEER(__CODE);
        true -> 0 end)
).

-endif.
