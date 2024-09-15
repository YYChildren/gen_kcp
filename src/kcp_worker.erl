%%%-------------------------------------------------------------------
%%% @author yangchaojun
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 四月 2020 16:06
%%%-------------------------------------------------------------------
-module(kcp_worker).
-author("yangchaojun").
-include("kcp.hrl").
-include("kcp_misc.hrl").

%% API
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([system_continue/3, system_terminate/4,
         write_debug/3,
         system_get_state/1, system_replace_state/2]).
-export([loop/3,do_send_msg/1]).
-export([check_valid/1]).

-record(state, {}).

start(#arg_kcp_worker_start{} = Arg) ->
    supervisor:start_child(kcp_worker_sup, [Arg]).

start_link(Arg) ->
    proc_lib:start_link(?MODULE, init, [[erlang:self(), Arg]]).

init([Parent, #arg_kcp_worker_start{spid = SPid, sport = SPort, socket = Socket, peer = PeerAddress, conv = Conv , handler = Handler}]) ->
    process_flag(trap_exit, true),
    SRef = erlang:monitor(process, SPid),
    kcp_data:set_server(SPid, SRef),
    kcp_data:set_conv(Conv),
    kcp_data:set_peer_address(PeerAddress),
    kcp_data:set_port(SPort),
    kcp_data:set_socket(Socket),
    kcp_data:set_handler(Handler),
    Deb = sys:debug_options([]),
    kcp_misc:kcp_pre_init(Conv),
    erlang:send(erlang:self(), start_service),
    proc_lib:init_ack(Parent, {ok, erlang:self()}),
    ?MODULE:loop(Parent, Deb, #state{}).

system_continue(Parent, Deb, State) ->
    ?MODULE:loop(Parent, Deb, State).

system_terminate(Reason, Parent, Deb, State) ->
    ?TRY_CATCH(terminate(Reason, Parent, Deb, State)),
    erlang:exit(normal).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, State, NState}.

write_debug(Dev, Event, State) ->
    io:format(Dev, "~w event = ~w~n", [State, Event]).

terminate(Reason, _Parent, _Deb, _State) ->
    Conv = kcp_data:get_conv(),
    Port = kcp_data:get_port(),
    Socket = kcp_data:get_socket(),
    Peer = kcp_data:get_peer_address(),
    case kcp_data:erase_kcp() of
        undefined ->
            Fmt = "", KcpState = [];
        Kcp ->
            Fmt = ", KcpState:~w",
            KcpState = [
                {waitsnd, erlang:element(2, gen_kcp:get_waitsnd(Kcp))},
                {waitrcv, erlang:element(2, gen_kcp:get_waitrcv(Kcp))},
                {peeksize,erlang:element(2, gen_kcp:get_peeksize(Kcp))},
                {wndsize, erlang:element(2, gen_kcp:get_wndsize(Kcp))},
                {nodelay, erlang:element(2, gen_kcp:get_nodelay(Kcp))},
                {mtu,     erlang:element(2, gen_kcp:get_mtu(Kcp)    )}
            ],
            catch gen_kcp:close(Kcp)
    end,
    kcp_misc:kcp_post_terminate(Conv),
    Fmt2 = "Kcp Worker exit:~w, Conv:~w, Port:~w, Peer: ~w, Socket:~w, Reason:~w",
    Args = [erlang:self(), Conv, Port, Peer, Socket, Reason],
    ?IF(Fmt =:= "", ?KCP_WARNING_MSG(Fmt2, Args), ?KCP_WARNING_MSG(Fmt2 ++ Fmt, Args ++ [KcpState])).

loop(Parent, Deb, State) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);
        {'$gen_call', {From, Mref}, Request} ->
            case handle_call(Request, From, State) of
                {reply, Reply, State2} ->
                    gen_server:reply({From, Mref}, Reply),
                    ?MODULE:loop(Parent, Deb, State2)
            end;
        {'EXIT', Parent, Reason} ->
            system_terminate(Reason, Parent, Deb, State);
        Info ->
            case handle_info(Info,State) of
                {noreply, State2} ->
                    ?MODULE:loop(Parent, Deb, State2);
                {stop, Reason, State2} ->
                    system_terminate(Reason, Parent, Deb, State2)
            end
    end.

handle_call(Request, _From, State) ->
    Reply = ?DO_HANDLE_CALL(Request, State),
    {reply, Reply, State}.

handle_info({exit, Reason}, State) ->
    {stop, Reason, State};
handle_info(Info, State) ->
    ?DO_HANDLE_INFO(Info, State),
    {noreply, State}.

do_handle({send_msg, Msg}) ->
    do_send_msg(Msg);
do_handle({udp, Address, Port, Data}) ->
    do_handle_udp(Address, Port, Data);
do_handle({kcp_reply,_Kcp,?KCP_MSG_SEND,ok}) ->
    ok;
do_handle({kcp_reply,_Kcp,?KCP_MSG_INPUT,ok}) ->
    ok;
do_handle({kcp_async, Kcp, _LRef, {ok, Msg}}) ->
    async_recv(Kcp),
    Conv = kcp_data:get_conv(),
    kcp_misc:kcp_handle_msg(Conv, Msg);
do_handle({kcp_reply,_Kcp,?KCP_MSG_SEND,{error, Reason}}) ->
    do_close(Reason);
do_handle({kcp_reply,_Kcp,?KCP_MSG_INPUT,{error, Reason}}) ->
    do_close(Reason);
do_handle({kcp_async, _Kcp, _LRef, {error, Reason}}) ->
    do_close(Reason);
do_handle({'EXIT', Kcp, Reason} = Info) ->
    case kcp_data:get_kcp() of
        Kcp -> do_close(Reason);
        _ -> kcp_misc:kcp_do_handle(Info)
    end;
do_handle({kcp_output, _Kcp, Binary}) ->
    Conv = kcp_data:get_conv(),
    case kcp_misc:kcp_pack(Conv, Binary) of
        {ok, Data} ->
            Socket = kcp_data:get_socket(),
            {Address, Port} = kcp_data:get_peer_address(),
            ?KCP_TRY_EXPR(kcp_throttle:update_counter_send(kcp_data:get_port())),
            kcp_inet:send(Socket, Address, Port, Data);
        {error, closed} -> %% 由于某种原因，需要关闭连接了
            do_close(normal)
    end;
do_handle({inet_reply,_S,ok}) ->
    ok;
do_handle({inet_reply,_S,_Reply} = Info) ->
    ?ERROR_MSG("发送UDP数据失败:~w", [Info]),
    ok;
do_handle(start_service) ->
    do_start_service();
do_handle({'DOWN', _MonitorRef, process, _Pid, Reason}) ->
    do_close(Reason);
do_handle({close, Reason}) ->
    do_close(Reason);
do_handle({func, F}) ->
    F();
do_handle({func, Time, F}) when erlang:is_function(F) ->
    case erlang:system_time(second) - Time > ?CALL_TIMETOUT of
        true ->
            {error, timeout};
        false ->
            F()
    end;
do_handle({func, M, F, A}) ->
    erlang:apply(M, F, A);
do_handle(Info) ->
    kcp_misc:kcp_do_handle(Info).

do_send_msg(Msg) when erlang:is_binary(Msg) ->
    case kcp_data:is_closing() of
        true -> reject;
        false ->
            case gen_kcp:async_send(kcp_data:get_kcp(), Msg) of
                ok ->
                    kcp_misc:kcp_post_send_msg(),
                    ok;
                {error, Reason} ->
                    do_close(Reason),
                    error
            end
    end;
do_send_msg(_Msg) ->
    reject.

do_handle_udp(Address, Port, Data) ->
    case kcp_data:get_peer_address() of
        {Address, Port} ->
            do_handle_udp(Data);
        undefined ->
            kcp_data:set_peer_address(Address),
            do_handle_udp(Data);
        _ -> %% NAT 出口变更了
            case do_handle_udp(Data) of
                ok -> kcp_data:set_peer_address(Address), ok;
                reject -> reject
            end
    end.

do_handle_udp(Data) ->
    case check_valid(Data) of
        true ->
            Kcp = kcp_data:get_kcp(),
            gen_kcp:async_input(Kcp, Data),
            ok;
        false ->
            reject
    end.

%% @doc 检查Conv 包是否合法
check_valid(Data) when erlang:byte_size(Data) > ?KCP_MTU_DEF -> false;
check_valid(<<>>) -> false;
check_valid(Data) ->
    Conv = kcp_data:get_conv(),
    check_valid2(Conv, Data, true).

check_valid2(Conv, ?KCP_SEG(Conv, Cmd, _Frg, _Wnd, _Ts, _Sn, _Una, Len, _Data, Left) = Pack, true) when ?IS_KCP_CMD(Cmd, Len) ->
    check_valid2(Conv, Left, kcp_misc:kcp_check_pack(Conv, Pack));
check_valid2(_Conv, <<>>, true) ->
    true;
check_valid2(_Conv, _, _) ->
    false.

do_start_service() ->
    Conv = kcp_data:get_conv(),
    case gen_kcp:open(Conv) of
        {ok, Kcp} ->
            {SndWnd, RcvWnd} = kcp_misc:kcp_windsize(),
            {NoDelay, Interval, Resend, Nc} = kcp_misc:kcp_nodelay(),
            Mtu = kcp_misc:kcp_mtu(),
            %% 主要参数设置
            gen_kcp:wndsize(Kcp, SndWnd, RcvWnd),
            gen_kcp:nodelay(Kcp, NoDelay, Interval, Resend, Nc),
            gen_kcp:set_mtu(Kcp, Mtu),
            async_recv(Kcp),
            kcp_data:set_kcp(Kcp),
            kcp_misc:kcp_post_init(Conv);
        {error, Reason} ->
            do_close(Reason)
    end.

async_recv(Kcp) ->
    case gen_kcp:async_recv(Kcp) of
        {ok, Ref} -> kcp_data:set_kcp_ref(Ref);
        {error, Reason} -> do_close(Reason)
    end.

do_close(Reason) ->
    case kcp_data:is_closing() of
        true ->
            ok;
        _ ->
            %% cloing 状态下只关闭发送端，保留接收端
            kcp_data:set_closing(),
            Conv = kcp_data:get_conv(),
            ?TRY_CATCH(kcp_misc:kcp_pre_terminate(Conv)),
            kcp_misc:exit(Reason)
    end.