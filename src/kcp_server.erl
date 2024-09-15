%%%-------------------------------------------------------------------
%%% @author yangchaojun
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 四月 2020 12:01
%%%-------------------------------------------------------------------
-module(kcp_server).
-author("yangchaojun").
-include("kcp.hrl").
-include("kcp_misc.hrl").

%% API
-export([start/2, stop/1, start_worker/3]).
-export([start_link/4]).
-export([init/1]).
-export([system_continue/3, system_terminate/4,
         write_debug/3,
         system_get_state/1, system_replace_state/2]).
-export([loop/3]).
-export([pname/1]).

-define(BUFFER_SIZE, (2 * 1024 * 1024)).
-define(UDP_OPTIONS, [binary,{active, false},{buffer,?BUFFER_SIZE},{sndbuf, ?BUFFER_SIZE div 2},{recbuf, ?BUFFER_SIZE div 2}]).
-record(state, {}).

start(Port, Handler) ->
    case gen_udp:open(Port, ?UDP_OPTIONS) of
        {ok, Socket} ->
            case inet:port(Socket) of
                {ok, RealPort} ->
                    Name = pname(RealPort),
                    case supervisor:start_child(kcp_server_sup, [Name, RealPort, Socket, Handler]) of
                        {ok, Pid} ->
                            gen_udp:controlling_process(Socket, Pid),
                            erlang:send(Pid, start_service),
                            {ok, Pid};
                        {error, Reason} ->
                            gen_udp:close(Socket),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    gen_udp:close(Socket),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
stop(RealPort) ->
    Name = pname(RealPort),
    case erlang:whereis(Name) of
        undefined ->
            ok;
        PID ->
            supervisor:terminate_child(kcp_server_sup, PID)
    end.
start_worker(SPid, Conv, PeerAddress) ->
    gen_server:call(SPid, {start_worker, Conv, PeerAddress}).

start_link(Name, Port, Socket, Handler) ->
    proc_lib:start_link(?MODULE, init, [[erlang:self(), Name, Port, Socket, Handler]], infinity, [{priority, high}]).

init([Parent, Name, Port, Socket, Handler]) ->
    process_flag(trap_exit, true),
    erlang:register(Name, erlang:self()),
    Deb = sys:debug_options([]),
    kcp_data:set_name(Name),
    kcp_data:set_port(Port),
    kcp_data:set_socket(Socket),
    kcp_data:set_handler(Handler),
    kcp_throttle:reg(Port),
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

terminate(Reason, _Parent, _Deb, #state{} = _State) ->
    Port = kcp_data:get_port(),
    kcp_throttle:dereg(Port),
    case kcp_data:erase_socket() of
         undefined = Socket ->
            ok;
        Socket ->
            catch erlang:port_close(Socket)
     end,
    ?INFO_MSG("Kcp Server exit:~w, Port:~w, Socket:~w, Reason:~w", [erlang:self(), Port, Socket, Reason]).

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

do_handle({inet_async, _S, _LRef, {ok, [F | AddrData]}}) ->
    ?KCP_TRY_EXPR(kcp_throttle:update_counter_recv(kcp_data:get_port())),
    Socket = kcp_data:get_socket(),
    % Success, UDP:
    async_recvfrom(Socket),
    {ok, InetRecvData} = case kcp_inet:get_addr(F, AddrData) of
        {{Family, _} = Addr, Data} when is_atom(Family) ->
            {ok, {Addr, 0, Data}};
        {{CIP, CPort}, Data} ->
            {ok, {CIP, CPort, Data}}
    end,
    route(InetRecvData);
%%do_handle({inet_async, S, Ref, {ok, {[F,P1,P0 | Addr], AncData, DE}}}) ->
%%    % Success, SCTP:
%%    {IP, _} = get_ip(F, Addr),
%%    {ok, {IP, ?u16(P1, P0), AncData, DE}};
do_handle({inet_async, _S, _LRef, {error, Reason}}) ->
    % Back-end error:
    kcp_misc:exit(Reason);
do_handle(start_service) ->
    Socket = kcp_data:get_socket(),
    async_recvfrom(Socket);
do_handle({start_worker, Conv, PeerAddress}) ->
    SPid = erlang:self(),
    SPort = kcp_data:get_port(),
    Socket = kcp_data:get_socket(),
    Handler = kcp_data:get_handler(),
    Arg = #arg_kcp_worker_start{spid = SPid, sport = SPort, socket = Socket, peer = PeerAddress, conv = Conv , handler = Handler},
    do_start_worker(Arg);
do_handle({close, Reason}) ->
    kcp_misc:exit(Reason);
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
    ?INFO_MSG("~p unknown msg: ~p", [?MODULE, Info]),
    ok.

async_recvfrom(Socket) ->
    case kcp_inet:async_recvfrom(Socket) of
        {ok, Ref} -> kcp_data:set_sock_ref(Ref);
        {error, Reason} -> kcp_misc:exit(Reason)
    end.

pname(Port) ->
    erlang:list_to_atom(lists:concat([?MODULE_STRING, "_", Port])).

route({Address, CPort, InetRecvData} = APData) ->
    SPort = kcp_data:get_port(),
    case kcp_misc:kcp_unpack(InetRecvData) of
        {ok, Conv, Data} ->
            case kcp_misc:kcp_worker_pid(Conv) of
                undefined ->
                    SPid = erlang:self(),
                    Socket = kcp_data:get_socket(),
                    PeerAddress = {Address, CPort},
                    Handler = kcp_data:get_handler(),
                    Arg = #arg_kcp_worker_start{spid = SPid, sport = SPort, socket = Socket, peer = PeerAddress, conv = Conv , handler = Handler},
                    case do_start_worker(Arg) of
                        {ok, WPid} ->
                            erlang:send(WPid, {udp, Address, CPort, Data});
                        {error, Reason} ->
                            ?WARNING_MSG("启动Worker失败：~w:~w", [Reason,{Conv, Address, CPort}])
                    end;
                Pid ->
                    erlang:send(Pid, {udp, Address, CPort, Data})
            end,
            ok;
        {error, Reason} ->
            ?INFO_MSG("非法数据包：~w <- ~w",[Reason, APData]),
            reject
    end.

do_start_worker(Arg) ->
    kcp_worker:start(Arg).
