%%%-------------------------------------------------------------------
%%% @author yangchaojun
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 五月 2020 14:14
%%%-------------------------------------------------------------------
-module(kcp_data).
-author("yangchaojun").

%% API
-export([
    get_name/0,
    set_name/1,
    get_port/0,
    set_port/1,
    get_handler/0,
    set_handler/1
]).
-export([
    get_socket/0,
    set_socket/1,
    erase_socket/0,
    get_sock_ref/0,
    set_sock_ref/1
]).
-export([
    get_kcp/0,
    set_kcp/1,
    erase_kcp/0,
    get_conv/0,
    set_conv/1,
    get_server/0,
    set_server/2,
    get_peer_address/0,
    set_peer_address/1,
    is_closing/0,
    set_closing/0,
    get_kcp_ref/0,
    set_kcp_ref/1,
    erase_kcp_ref/0
]).

%% --------------------- public begin -------------------------

get_name() ->
    erlang:get(name).
set_name(undefined) ->
    erlang:erase(name);
set_name(Name) ->
    erlang:put(name, Name).

get_port() ->
    erlang:get(port).
set_port(Port) ->
    erlang:put(port, Port).

get_handler() ->
    erlang:get(handler).
set_handler(undefined) ->
    erlang:erase(handler);
set_handler(Handler) ->
    erlang:put(handler, Handler).

%% --------------------- server begin -------------------------

get_socket() ->
    erlang:get(socket).
set_socket(Socket) ->
    erlang:put(socket, Socket).
erase_socket() ->
    erlang:erase(socket).

get_sock_ref() ->
    erlang:get(sock_ref).
set_sock_ref(SockRef) ->
    erlang:put(sock_ref, SockRef).

%% --------------------- worker begin -------------------------

get_kcp() ->
    erlang:get(kcp).
set_kcp(Kcp) ->
    erlang:put(kcp, Kcp).
erase_kcp() ->
    erlang:erase(kcp).

get_conv() ->
    erlang:get(conv).
set_conv(Conv) ->
    erlang:put(conv, Conv).

get_server() ->
    erlang:get(server).
set_server(SPid, SRef) ->
    erlang:put(server, {SPid, SRef}).

get_peer_address() ->
    erlang:get(peer_address).
set_peer_address(Address) ->
    erlang:put(peer_address, Address).

is_closing() ->
    erlang:get(closing) =:= true.
set_closing() ->
    erlang:put(closing, true).

get_kcp_ref() ->
    erlang:get(kcp_ref).
set_kcp_ref(UpdateRef) ->
    erlang:put(kcp_ref, UpdateRef).
erase_kcp_ref() ->
    erlang:erase(kcp_ref).