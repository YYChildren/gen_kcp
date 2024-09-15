%%%-------------------------------------------------------------------
%%% @author yangchaojun
%%% @copyright (C) 2020, <www.mingchao.com>
%%% @doc
%%%
%%% @end
%%% Created : 16. 十二月 2020 15:30
%%%-------------------------------------------------------------------
-module(kcp_throttle).
-author("yangchaojun").
-include("mgee.hrl").

-behaviour(gen_server).

-export([
    i/0,ii/0,
    i/1,
    i/2
]).

-export([
    reg/1,
    dereg/1,
    update_counter_send/1,
    update_counter_recv/1
]).

-export([
    start_link/0
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(ETS_KCP_STATS, ets_kcp_stats).
-define(KCP_COUNTER_INTERVAL, 5). %% 秒, 刷新周期
-define(DECAY_FACTOR, 0.7). %% EMA的衰减率, 取值区间(0, 1], 减小会让延迟值更平滑, 加大会让延迟值反应更灵敏
-define(DECAY_SENSITIVITY, 2). %% 采样波动的放大倍数, 取值>0, 取1时不做偏移, >1会对数值的上升更灵敏, <1会对数值的下降更灵敏
-define(KCP_QPS_COUNT, 12). %% 个, QPS清单长度
-define(GLOBAL_PORT, global).
-define(IO_SEND, send).
-define(IO_RECV, recv).
-define(ALL_IO, [?IO_SEND, ?IO_RECV]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(Arg) ->
    do_init(Arg).

handle_call(Request, _From, State) ->
    Reply = ?DO_HANDLE_CALL(Request, State),
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    ?DO_HANDLE_INFO(Msg, State),
    {noreply, State}.

handle_info({'EXIT', _, _Reason}, State) ->
    {stop, normal, State};
handle_info(Info, State) ->
    ?DO_HANDLE_INFO(Info, State),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_init(_) ->
    ets:new(?ETS_KCP_STATS, [named_table, set, public, {read_concurrency, true}, {write_concurrency, true}]),
    schedule_recalc(),
    {ok, undefined}.

do_handle(maybe_recalc_counter) ->
    maybe_recalc_counter();
do_handle({reg, Port}) ->
    do_reg(Port);
do_handle({dereg, Port}) ->
    do_dereg(Port);
do_handle({'DOWN', _MonitorRef, process, Process, _Info}) ->
    remove_reg(Process);
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
    ?INFO_MSG("unknown: ~w", [Info]).

i() ->
    i(?GLOBAL_PORT).

ii() ->
    [Info || Port <- [?GLOBAL_PORT | get_reg_list()], Info <- i(Port)].

i(Port) ->
    [i(IO, Port) || IO <- ?ALL_IO].

i(IO, Port) ->
    [{port, Port},{io, IO}, {qps, get_qps(IO, Port)}, {counter, get_counter(IO, Port)}, {smooth_counter, get_smooth_counter(IO, Port)}].

reg(Port) ->
    gen_server:call(?MODULE, {reg, Port}).
do_reg(Port) ->
    add_reg(Port).

dereg(Port) ->
    gen_server:call(?MODULE, {reg, Port}).

do_dereg(Port) ->
    remove_reg(Port).

add_reg(Port) ->
    case is_reg(Port) of
        true ->
            ok;
        _ ->
            RegList = get_reg_list(),
            set_reg_list([Port | RegList]),
            set_reg(Port),
            ok
    end.

remove_reg(Port) ->
    case is_reg(Port) of
        true ->
            RegList = get_reg_list(),
            unset_reg(Port),
            set_reg_list([Port2 || Port2 <- RegList, Port2 =/= Port]);
        _ ->
            ok
    end.

update_counter_send(Port) ->
    update_counter(?IO_SEND, Port).

update_counter_recv(Port) ->
    update_counter(?IO_RECV, Port).

%% 使用整 ?KCP_COUNTER_INTERVAL 时间做处理
-define(NEXT_TIME(__NOW), (((__NOW) + ?KCP_COUNTER_INTERVAL) div ?KCP_COUNTER_INTERVAL * ?KCP_COUNTER_INTERVAL)).
schedule_recalc() ->
    case erlang:get(send_recalc) of
        true ->
            ok;
        _ ->
            Now = mtime:now(),
            erlang:send_after((?NEXT_TIME(Now) - Now) * 1000, self(), maybe_recalc_counter),
            erlang:put(send_recalc, true)
    end,
    ok.

maybe_recalc_counter() ->
    erlang:erase(send_recalc),
    schedule_recalc(),
    Now = mtime:now(),
    case erlang:get(recalc_counter) of
        undefined ->
            erlang:put(recalc_counter, Now),
            recalc_counter(0);
        T when ?NEXT_TIME(T) =< Now ->
            erlang:put(recalc_counter, Now),
            recalc_counter(Now - T);
        _ ->
            ignore
    end.

recalc_counter(Elapsed) ->
    [begin
        ?IF(Count1 =/= 0, decrease_counter(IO, Port, Count1), ok),
        DecayFactor = calc_decay_factor(Count0, Count1, ?DECAY_SENSITIVITY),
        Count = calc_decay(Count0, Count1, DecayFactor),
        set_smooth_counter(IO, Port, Count),
        case Elapsed of
            0 -> ignore;
            _ ->
                QPS = erlang:trunc(Count / Elapsed),
                QPSs = get_qps(IO, Port),
                QPSs2 = drop_tail_zeroes([QPS|QPSs]),
                set_qps(IO, Port, lists:sublist(QPSs2, ?KCP_QPS_COUNT))
        end
     end || IO <- ?ALL_IO, Port <- [?GLOBAL_PORT | get_reg_list()], begin Count0 = get_smooth_counter(IO, Port), Count1 = get_counter(IO, Port), Count0 =/= 0 orelse Count1 =/= 0 end],
    ok.

drop_tail_zeroes(QPSs0) ->
    QPSs1 = lists:reverse(QPSs0),
    QPSs2 = lists:dropwhile(fun(C) -> C =:= 0 end, QPSs1),
    lists:reverse(QPSs2).


%% @doc 根据Sensitivity计算修正的衰减率
%% @returns AdjusctedDecayFactor := (0, 1)
calc_decay_factor(Old, New, _) when Old == New -> 0;
calc_decay_factor(Old, New, Sen) when Old < New, Sen > 0, Sen =< 1 ->
    ?DECAY_FACTOR * Sen;
calc_decay_factor(Old, New, Sen) when Old < New, Sen > 1 ->
    ?DECAY_FACTOR + (1 - ?DECAY_FACTOR) * (1 - 1 / Sen);
calc_decay_factor(Old, New, Sen) when Old > New, Sen > 0, Sen =< 1 ->
    (?DECAY_FACTOR - 1) * Sen + 1;
calc_decay_factor(Old, New, Sen) when Old > New, Sen > 1 ->
    ?DECAY_FACTOR / Sen.

calc_decay(Value, Value, _) -> Value;
calc_decay(Old, New, DecayFactor) ->
    erlang:trunc(Old * (1 - DecayFactor) + New * DecayFactor).

%% -----------------------------------------------------

-define(GET_VALUE(__K, __D), case ets:lookup(?ETS_KCP_STATS, __K) of [{_, __V}] -> __V; [] -> __D end).
-define(GET_VALUE(__K), ?GET_VALUE(?ETS_KCP_STATS, __K, undefined)).
-define(SET_VALUE(__K, __V), ets:insert(?ETS_KCP_STATS, {__K, __V})).
-define(DEL_VALUE(__K), case ets:lookup(?ETS_KCP_STATS, __K) of [{_, __V}] -> ets:delete(?ETS_KCP_STATS, __K), __V; [] -> undefined end).
-define(UPDATE_COUNTER(__K, __INCR, __D), ets:update_counter(?ETS_KCP_STATS, __K, __INCR, {__K, __D})).
-define(UPDATE_COUNTER(__K, __INCR), ets:update_counter(?ETS_KCP_STATS, __K, __INCR)).

-define(MAKE_KEY(__K1, __K2, __K3), {__K1, __K2, __K3}).
-define(MAKE_KEY(__K1, __K2), {__K1, __K2}).

-define(DK_REG, reg).
is_reg(Port) ->
    ?GET_VALUE(?MAKE_KEY(?DK_REG, Port), false).
set_reg(Port) ->
    ?SET_VALUE(?MAKE_KEY(?DK_REG, Port), true).
unset_reg(Port) ->
    ?DEL_VALUE(?MAKE_KEY(?DK_REG, Port)).

-define(DK_REG_LIST, reg_list).
get_reg_list() ->
    ?GET_VALUE(?DK_REG_LIST, []).
set_reg_list([]) ->
    ?DEL_VALUE(?DK_REG_LIST);
set_reg_list(RegList) ->
    ?SET_VALUE(?DK_REG_LIST, RegList).

-define(DK_COUNTER, counter).
update_counter(IO, Port) ->
    ?UPDATE_COUNTER(?MAKE_KEY(?DK_COUNTER, IO, Port), 1, 1),
    ?UPDATE_COUNTER(?MAKE_KEY(?DK_COUNTER, IO, ?GLOBAL_PORT), 1, 1).

decrease_counter(IO, Port, Count) ->
    ?UPDATE_COUNTER(?MAKE_KEY(?DK_COUNTER, IO, Port), -Count).

get_counter(IO, Port) ->
    ?GET_VALUE(?MAKE_KEY(?DK_COUNTER, IO, Port), 0).

-define(DK_QPS, qps).
get_qps(IO, Port) ->
    ?GET_VALUE(?MAKE_KEY(?DK_QPS, IO, Port), []).
set_qps(IO, Port, []) ->
    ?DEL_VALUE(?MAKE_KEY(?DK_QPS, IO, Port));
set_qps(IO, Port, Qps) ->
    ?SET_VALUE(?MAKE_KEY(?DK_QPS, IO, Port), Qps).

-define(DK_SMOOTH_COUNTER, smooth_counter).
get_smooth_counter(IO, Port) ->
    ?GET_VALUE(?MAKE_KEY(?DK_SMOOTH_COUNTER, IO, Port), 0).
set_smooth_counter(IO, Port, SmoothCounter) ->
    ?SET_VALUE(?MAKE_KEY(?DK_SMOOTH_COUNTER, IO, Port), SmoothCounter).