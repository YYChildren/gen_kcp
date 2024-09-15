%%%-------------------------------------------------------------------
%%% @author yangchaojun
%%% @copyright (C) 2020, <www.mingchao.com>
%%% @doc
%%%
%%% @end
%%% Created : 14. 5月 2020 14:08
%%%-------------------------------------------------------------------
-module(gen_kcp).
-include("kcp.hrl").

%% API
-export([
    start/0,
    stop/0,
    load/0,
    unload/0,
    open/1,
    close/1,
    wndsize/3,
    nodelay/5,
    set_mtu/2,
    send/2,
    async_send/2,
    input/2,
    async_input/2,
    recv/1,
    async_recv/1,
    get_output/1,
    get_waitsnd/1,
    get_waitrcv/1,
    get_wndsize/1,
    get_nodelay/1,
    get_mtu/1,
    get_peeksize/1
]).

-define(APPS, [gen_kcp]).

start() ->
    ok = mlib_misc:start_applications(?APPS).

stop() ->
    ok = mlib_misc:stop_applications(?APPS).

load() ->
    Path = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    filename:join([filename:dirname(filename:dirname(Filename)),"priv"]);
                _ ->
                    filename:join(["./priv"])
            end;
        Dir ->
            Dir
    end,
    case erl_ddll:load_driver(Path, ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, ErrorDesc} -> erlang:exit({error, ErrorDesc})
    end.

unload() ->
    erl_ddll:unload_driver(?MODULE).

-spec open(Conv :: non_neg_integer()) -> {ok, Kcp :: port()} | {error, Reason::term()}.
open(Conv) ->
    try erlang:open_port({spawn_driver,?MODULE}, [binary]) of
        Kcp ->
            case kcp_ctl(Kcp, ?KCP_CTL_CREATE, <<Conv:32>>) of
                {ok,_} -> {ok,Kcp};
                {error,_}=E1 ->
                    close(Kcp),
                    E1
            end
    catch
        %% The only (?) way to get here is to try to open
        %% the sctp driver when it does not exist (badarg)
        error:badarg       -> {error, eprotonosupport};
        %% system_limit if out of port slots
        error:system_limit -> {error, system_limit}
    end.

-spec close(Kcp :: port()) -> ok.
close(Kcp) ->
    catch erlang:port_close(Kcp),
    receive {'EXIT',Kcp,_} -> ok after 0 -> ok end.

%% @doc set maximum window size: sndwnd=32, rcvwnd=32 by default
-spec wndsize(port(), non_neg_integer(), non_neg_integer()) -> ok | {error, Reason :: term()}.
wndsize(Kcp, SndWnd, RcvWnd) when erlang:is_port(Kcp) ->
    case kcp_ctl(Kcp, ?KCP_CTL_WNDSIZE, <<SndWnd:32, RcvWnd:32>>) of
        {ok, _} -> ok;
        {error,_}=Error -> Error
    end.

%% @doc
%% fastest: ikcp_nodelay(kcp, 1, 20, 2, 1)
%% nodelay: 0:disable(default), 1:enable
%% interval: internal update timer interval in millisec, default is 100ms
%% resend: 0:disable fast resend(default), 1:enable fast resend
%% nc: 0:normal congestion control(default), 1:disable congestion control
%% @end
-spec nodelay(port(), non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    ok | {error, Reason :: term()}.
nodelay(Kcp, NoDelay, InterVal, ReSend, Nc) when erlang:is_port(Kcp) ->
    case kcp_ctl(Kcp, ?KCP_CTL_NODELAY, <<NoDelay:32, InterVal:32, ReSend:32, Nc:32>>) of
        {ok, _} -> ok;
        {error,_}=Error -> Error
    end.

%% @doc change MTU size, default is 1400
-spec set_mtu(port(), integer()) -> ok | {error, Reason :: term()}.
set_mtu(Kcp, Mtu) when erlang:is_port(Kcp) ->
    case kcp_ctl(Kcp, ?KCP_CTL_SETMTU, <<Mtu:32>>) of
        {ok, _} -> ok;
        {error,_}=Error -> Error
    end.

-spec recv(port()) -> {ok, Binary :: binary()} | {error, Reason :: term()}.
recv(Kcp) when erlang:is_port(Kcp) ->
    case async_recv(Kcp) of
        {ok, <<Ref:32>>} ->
            receive
                {kcp_async, Kcp, Ref, {ok, Binary}} ->
                    {ok, Binary};
                {kcp_async, Kcp, Ref, {error, Reason}} ->
                    {error, Reason};
                {'EXIT', Kcp, _Reason} ->
                    {error, closed}
            end;
        {error,_}=Error -> Error
    end.

-spec async_recv(port()) -> {ok, Ref :: non_neg_integer()} | {error, Reason :: term()}.
async_recv(Kcp) when erlang:is_port(Kcp) ->
    case kcp_ctl(Kcp, ?KCP_CTL_REQRECV, <<>>) of
        {ok, <<Ref:32>>} -> {ok, Ref};
        {error, _} = Error -> Error
    end.

-spec send(Kcp, Packet) -> ok | {error, Reason} when Kcp :: port(), Packet :: iodata(), Reason :: term().
send(Kcp, Packet) when erlang:is_port(Kcp) ->
    case async_send(Kcp, Packet) of
        ok ->
            receive
                {kcp_reply,Kcp,?KCP_MSG_SEND,Status} ->
                    Status;
                {'EXIT', Kcp, _Reason} ->
                    {error, closed}
            end;
        {error, _} = Error -> Error
    end.

async_send(Kcp, Packet) ->
    kcp_msg(Kcp, ?KCP_MSG_SEND, Packet).

-spec input(Kcp, Packet) -> ok | {error, Reason} when Kcp :: port(), Packet :: iodata(), Reason :: term().
input(Kcp, Packet) ->
    case async_input(Kcp, Packet) of
        ok ->
            receive
                {kcp_reply,Kcp,?KCP_MSG_INPUT,Status} ->
                    Status;
                {'EXIT', Kcp, _Reason} ->
                    {error, closed}
            end;
        {error, _} = Error -> Error
    end.

async_input(Kcp, Packet) ->
    kcp_msg(Kcp, ?KCP_MSG_INPUT, Packet).

%% @doc get the output data that need to sendto udp
-spec get_output(Kcp) -> Binary when Kcp :: port(), Binary :: binary().
get_output(Kcp) ->
    receive
        {kcp_output, Kcp, Binary} ->
            Binary
    end.

get_waitsnd(Kcp) when erlang:is_port(Kcp) ->
    case kcp_ctl(Kcp, ?KCP_CTL_WAITSND, <<>>) of
        {ok, <<WaitSnd:32>>} -> {ok, WaitSnd};
        {error, _} = Error -> Error
    end.

get_waitrcv(Kcp) when erlang:is_port(Kcp) ->
    case kcp_ctl(Kcp, ?KCP_CTL_WAITRCV, <<>>) of
        {ok, <<WaitRcv:32>>} -> {ok, WaitRcv};
        {error, _} = Error -> Error
    end.

get_wndsize(Kcp) when erlang:is_port(Kcp) ->
    case kcp_ctl(Kcp, ?KCP_CTL_GETWNDSIZE, <<>>) of
        {ok, <<SndWnd:32, RcvWnd:32>>} -> {ok, {SndWnd, RcvWnd}};
        {error, _} = Error -> Error
    end.

get_nodelay(Kcp) when erlang:is_port(Kcp) ->
    case kcp_ctl(Kcp, ?KCP_CTL_GETNODELAY, <<>>) of
        {ok, <<Nodelay:32, Interval:32, Resend:32, Nc:32>>} -> {ok, {Nodelay, Interval, Resend, Nc}};
        {error, _} = Error -> Error
    end.

get_mtu(Kcp) when erlang:is_port(Kcp) ->
    case kcp_ctl(Kcp, ?KCP_CTL_GETMTU, <<>>) of
        {ok, <<Mtu:32>>} -> {ok, Mtu};
        {error, _} = Error -> Error
    end.

get_peeksize(Kcp) when erlang:is_port(Kcp) ->
    case kcp_ctl(Kcp, ?KCP_CTL_PEEKSIZE, <<>>) of
        {ok, <<PeekSize:32/signed>>} -> {ok, PeekSize};
        {error, _} = Error -> Error
    end.

%% Control command
kcp_ctl(Port, Cmd, Args) ->
    Result =
    try erlang:port_control(Port, Cmd, Args) of
        <<?KCP_REP_OK, Reply/binary>>  -> {ok,Reply};
        <<?KCP_REP_ERROR, Err/binary>> -> {error,erlang:binary_to_atom(Err, latin1)}
    catch
        error:_               -> {error,einval}
    end,
    Result.

kcp_msg(Port, Cmd, Args) ->
    try erlang:port_command(Port, [Cmd, Args]) of
        false -> % Port busy and nosuspend option passed
            {error,busy};
        true ->
            ok
    catch
        error:_Error ->
             {error,einval}
    end.