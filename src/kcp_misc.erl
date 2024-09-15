%%%-------------------------------------------------------------------
%%% @author yangchaojun
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 四月 2020 11:58
%%%-------------------------------------------------------------------
-module(kcp_misc).
-author("yangchaojun").
-include("kcp_misc.hrl").

%% API
-export([
    close/1,
    close/2,
    exit/1,
    exit/2,
    send_msg/2
]).

%% 以下为内存进程调用
-export([
    %% 可选回调
    kcp_windsize/0,
    kcp_nodelay/0,
    kcp_mtu/0,
    kcp_pre_init/1,
    kcp_post_init/1,
    kcp_pre_terminate/1,
    kcp_post_terminate/1,

    %% 必选回调
    kcp_do_handle/1,
    kcp_handle_msg/2,
    kcp_post_send_msg/0,
    kcp_check_pack/2,
    kcp_pack/2,
    kcp_unpack/1,
    kcp_worker_pid/1
]).

close(Reason) ->
    erlang:send(erlang:self(),{close, Reason}).

close(Process, Reason)->
    erlang:send(Process,{close, Reason}).

%% @doc 退出进程
exit(Reason)->
    erlang:send(erlang:self(),{exit, Reason}).

exit(Process, Reason)->
    erlang:send(Process,{exit, Reason}).

send_msg(Worker, Bin) when erlang:is_binary(Bin) ->
    erlang:send(Worker, {send_msg, Bin}).

%% -----------------  以下函数需要自行实现  -----------------

kcp_windsize() ->
    ?KCP_CALLBACK([], {?KCP_SNDWND, ?KCP_RCVWND}).

kcp_nodelay() ->
    ?KCP_CALLBACK([], {?KCP_NODELAY, ?KCP_INTERVAL, ?KCP_RESEND, ?KCP_NC}).

kcp_mtu() ->
    ?KCP_CALLBACK([], ?KCP_MTU_DEF).

kcp_pre_init(Conv) ->
    ?KCP_CALLBACK([Conv]).

kcp_post_init(Conv) ->
    ?KCP_CALLBACK([Conv]).

kcp_pre_terminate(Conv) ->
    ?KCP_CALLBACK([Conv]).

kcp_post_terminate(Conv) ->
    ?KCP_CALLBACK([Conv]).

kcp_do_handle(Info) ->
    ?KCP_HANDLE(Info).

kcp_post_send_msg() ->
    ?KCP_HANDLE().

kcp_handle_msg(Conv, Msg) ->
    ?KCP_HANDLE(Conv, Msg).

kcp_check_pack(Conv, Binary) ->
    ?KCP_HANDLE(Conv, Binary).

kcp_pack(Conv, Binary) ->
    ?KCP_HANDLE(Conv, Binary).

kcp_unpack(InetRecvData) ->
    ?KCP_HANDLE(InetRecvData).

kcp_worker_pid(Conv) ->
    ?KCP_HANDLE(Conv).