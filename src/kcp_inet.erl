%%%-------------------------------------------------------------------
%%% @author yangchaojun
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 四月 2020 14:27
%%%-------------------------------------------------------------------
-module(kcp_inet).
-author("yangchaojun").
-include("mgee.hrl").
-include_lib("kernel/src/inet_int.hrl").

%% API
-export([
    send/4,
    async_recvfrom/1,
    get_addr/2,
    get_ip/2
]).

-ifdef(OTP_RELEASE).
send(S, Address, Port, Data) ->
    erlang:port_command(S, [enc_value_2(addr, {Address, Port}),enc_value_2(uint32, 0), [],Data]).
-else.
send(S, Address, Port, Data) ->
    erlang:port_command(S, [enc_value_2(addr, {Address, Port}), Data]).
-endif.

enc_value_2(uint32, Val)    -> ?int32(Val);
enc_value_2(addr, {any,Port}) ->
    [?INET_AF_ANY|?int16(Port)];
enc_value_2(addr, {loopback,Port}) ->
    [?INET_AF_LOOPBACK|?int16(Port)];
enc_value_2(addr, {IP,Port}) when tuple_size(IP) =:= 4 ->
    [?INET_AF_INET,?int16(Port)|ip4_to_bytes(IP)];
enc_value_2(addr, {IP,Port}) when tuple_size(IP) =:= 8 ->
    [?INET_AF_INET6,?int16(Port)|ip6_to_bytes(IP)];
enc_value_2(addr, {File,_}) when is_list(File); is_binary(File) ->
    [?INET_AF_LOCAL,iolist_size(File)|File];
%%
enc_value_2(addr, {inet,{any,Port}}) ->
    [?INET_AF_INET,?int16(Port)|ip4_to_bytes({0,0,0,0})];
enc_value_2(addr, {inet,{loopback,Port}}) ->
    [?INET_AF_INET,?int16(Port)|ip4_to_bytes({127,0,0,1})];
enc_value_2(addr, {inet,{IP,Port}}) ->
    [?INET_AF_INET,?int16(Port)|ip4_to_bytes(IP)];
enc_value_2(addr, {inet6,{any,Port}}) ->
    [?INET_AF_INET6,?int16(Port)|ip6_to_bytes({0,0,0,0,0,0,0,0})];
enc_value_2(addr, {inet6,{loopback,Port}}) ->
    [?INET_AF_INET6,?int16(Port)|ip6_to_bytes({0,0,0,0,0,0,0,1})];
enc_value_2(addr, {inet6,{IP,Port}}) ->
    [?INET_AF_INET6,?int16(Port)|ip6_to_bytes(IP)];
enc_value_2(addr, {local,Addr}) ->
    %% A binary is passed as is, but anything else will be
    %% regarded as a filename and therefore encoded according to
    %% the current system filename encoding mode.
    Bin =
    if
        is_binary(Addr) ->
        Addr;
        true ->
        unicode:characters_to_binary(
          Addr, file:native_name_encoding())
    end,
    [?INET_AF_LOCAL,byte_size(Bin),Bin].

ip4_to_bytes({A,B,C,D}) ->
    [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff].

ip6_to_bytes({A,B,C,D,E,F,G,H}) ->
    [?int16(A), ?int16(B), ?int16(C), ?int16(D),
     ?int16(E), ?int16(F), ?int16(G), ?int16(H)].

async_recvfrom(Socket) ->
    recvfrom0(Socket, 0, -1).

recvfrom0(S, Length, Time)
  when is_integer(Length), 0 =< Length, Length =< 16#ffffffff ->
    case ctl_cmd(S, ?PACKET_REQ_RECV,[enc_time(Time),?int32(Length)]) of
    {ok,[R1,R0]} ->
        Ref = ?u16(R1,R0),
        {ok, Ref};
    {error,_}=Error ->
        Error % Front-end error
    end;
recvfrom0(_, _, _) -> {error,einval}.

enc_time(Time) when Time < 0 -> [255,255,255,255];
enc_time(Time) -> ?int32(Time).

get_addr(?INET_AF_LOCAL, [N|Addr]) ->
    {A,Rest} = lists:split(N, Addr),
    {{local,iolist_to_binary(A)},Rest};
get_addr(?INET_AF_UNSPEC, Rest) ->
    {{unspec,<<>>},Rest};
get_addr(?INET_AF_UNDEFINED, Rest) ->
    {{undefined,<<>>},Rest};
get_addr(Family, [P1,P0|Addr]) ->
    {IP,Rest} = get_ip(Family, Addr),
    {{IP,?u16(P1, P0)},Rest}.

get_ip(?INET_AF_INET, Addr) ->
    get_ip4(Addr);
get_ip(?INET_AF_INET6, Addr) ->
    get_ip6(Addr).

get_ip4([A,B,C,D | T]) -> {{A,B,C,D},T}.

get_ip6([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16 | T]) ->
    { { ?u16(X1,X2),?u16(X3,X4),?u16(X5,X6),?u16(X7,X8),
    ?u16(X9,X10),?u16(X11,X12),?u16(X13,X14),?u16(X15,X16)},
      T }.

-ifdef(OTP_RELEASE).
-define(ERTS_INET_DRV_CONTROL_MAGIC_NUMBER, 16#03f1a300).
-else.
-define(ERTS_INET_DRV_CONTROL_MAGIC_NUMBER, 0).
-endif.

%% Control command
ctl_cmd(Port, Cmd, Args) ->
    ?DEBUG("prim_inet:ctl_cmd(~p, ~p, ~p)~n", [Port,Cmd,Args]),
    Result =
    try erlang:port_control(Port, Cmd+?ERTS_INET_DRV_CONTROL_MAGIC_NUMBER, Args) of
        [?INET_REP_OK|Reply]  -> {ok,Reply};
        [?INET_REP]  -> inet_reply;
        [?INET_REP_ERROR|Err] -> {error,list_to_atom(Err)}
    catch
        error:_               -> {error,einval}
    end,
        ?DEBUG("prim_inet:ctl_cmd() -> ~p~n", [Result]),
    Result.
