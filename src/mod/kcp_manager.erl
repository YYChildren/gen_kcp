%%%-------------------------------------------------------------------
%%% @author yangchaojun
%%% @copyright (C) 2020, <www.mingchao.com>
%%% @doc
%%%
%%% @end
%%% Created : 27. 五月 2020 9:12
%%%-------------------------------------------------------------------
-module(kcp_manager).
-author("yangchaojun").
-include("kcp_misc.hrl").

-behaviour(gen_server).

%% API
-export([
    start/1,
    stop/1,
    start_link/1
]).

-export([
    apply_conv/2,
    put_conv/4,
    close_conv/2,
    my_agent_key/2,
    reg_worker/3,
    worker_pid/2
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-export([
    i/2,
    call/2
]).
-record(state, {}).

-define(GET_VALUE(__T, __K, __D), case ets:lookup(__T, __K) of [{_, __V}] -> __V; [] -> __D end).
-define(GET_VALUE(__T, __K), ?GET_VALUE(__T, __K, undefined)).
-define(SET_VALUE(__T, __K, __V), ets:insert(__T, {__K, __V})).
-define(DEL_VALUE(__T, __K), case ets:lookup(__T, __K) of [{_, __V}] -> ets:delete(__T, __K), __V; [] -> undefined end).

-define(GET_ETS_TABLE(), ?ETS_KCP_MANAGER(kcp_data_custom:get_manager_name())).

%%%===================================================================
%%% API
%%%===================================================================
i(PName, Type) ->
    IFunc =
        fun() ->
            Table = ?GET_ETS_TABLE(),
            #{
                conv_pointer => get_pointer(Table),
                %% 一半conv为key，一半pid为key，再加1个pointer
                table => ?IF( Type=:=1 orelse ets:info(Table, size) =< 200, lists:sort(ets:tab2list(Table)), ets:info(Table, size))
            }
        end,
    call(PName, {func, IFunc}).
call(PName, Msg) ->
    gen_server:call(PName, Msg, infinity).

start(PName) ->
    case supervisor:start_child(kcp_sup, {PName, {?MODULE, start_link, [PName]}, permanent, 2000000, worker, [?MODULE]}) of
        {ok, P} -> {ok, P};
        {error, {already_started, P}} -> {ok, P};
        Error -> Error
    end.
stop(PName) ->
    case erlang:whereis(?MODULE) of
        undefined ->
            ok;
        _PID ->
            supervisor:terminate_child(kcp_sup, PName),
            supervisor:delete_child(kcp_sup, PName)
    end.

start_link(PName) ->
    gen_server:start_link({local, PName}, ?MODULE, [PName], []).

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

do_init([PName]) ->
    erlang:process_flag(trap_exit, true),
    kcp_data_custom:set_manager_name(PName),
    ets:new(?ETS_KCP_MANAGER(PName), [named_table,protected,set,{read_concurrency, true}]),
    {ok, #state{}}.


do_handle({apply_conv, AgentPID}) ->
    do_apply_conv(AgentPID);
do_handle({put_conv, AgentPID, Conv, Key}) ->
    do_put_conv(AgentPID, Conv, Key);
do_handle({close_conv, Conv}) ->
    do_close_conv(Conv);
do_handle({reg_worker, Conv, PID}) ->
    do_reg_worker(Conv, PID);
do_handle({'DOWN', _MonitorRef, process, DownPID, _Info}) ->
    maybe_close_conv(DownPID);
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

%% @doc 一般用于服务端，申请一个conv
apply_conv(PName, AgentPID) ->
    gen_server:call(PName, {apply_conv, AgentPID}, infinity).

do_apply_conv(AgentPID) ->
    Table = ?GET_ETS_TABLE(),
    case get_agent_aux(Table, AgentPID) of
        undefined ->
            Ptr = get_pointer(Table),
            case request_conv(Table, Ptr, Ptr) of
                undefined -> {error, full};
                Conv ->
                    Ref = erlang:monitor(process, AgentPID),
                    Key = mlib_tool:random(?GKCP_KEY_MAX),
                    set_agent_key(Table, Conv, {AgentPID, Key}),
                    set_agent_aux(Table, AgentPID, {Conv, Ref}),
                    set_pointer(Table, Conv),
                    {ok, Conv, Key}
            end;
        {Conv, _} ->
            {AgentPID, Key} = get_agent_key(Table, Conv),
            {ok, Conv, Key}
    end.

request_conv(Table, Begin, Ptr) ->
    Conv = (Ptr + 1) band ?GKCP_CONV_MAX,
    if
        Conv =:= Begin -> undefined;
        Conv =:= 0 -> request_conv(Table, Begin, Conv); %% 0 作为默认值，不使用
        true ->
            case get_agent_key(Table, Conv) of
                undefined -> Conv;
                _ -> request_conv(Table, Begin, Conv)
            end
    end.

%% @doc 一般用于客户端，记录一个conv
put_conv(PName, AgentPID, Conv, Key) ->
    gen_server:call(PName, {put_conv, AgentPID, Conv, Key}, infinity).

do_put_conv(_AgentPID, 0, _Key) -> %% 0 作为默认值，不使用
    {error, invalid_conv};
do_put_conv(AgentPID, Conv, Key) ->
    Table = ?GET_ETS_TABLE(),
    case get_agent_key(Table, Conv) of
        {AgentPID, Key} -> %% 重复
            ok;
        {AgentPID, _Key} -> %% 更新Key
            set_agent_key(Table, Conv, {AgentPID, Key}),
            ok;
        {_AgentPID, _Key} -> %% 其他进程在使用
            {error, convinuse};
        undefined ->
            Ref = erlang:monitor(process, AgentPID),
            set_agent_key(Table, Conv, {AgentPID, Key}),
            set_agent_aux(Table, AgentPID, {Conv, Ref}),
            ok
    end.

%% 关闭Conv
close_conv(PName, Conv) ->
    gen_server:call(PName, {close_conv, Conv}, infinity).

do_close_conv(Conv) ->
    Table = ?GET_ETS_TABLE(),
    case del_agent_key(Table, Conv) of
        undefined ->
            ok;
        {AgentPID, _Key} ->
            case del_agent_aux(Table, AgentPID) of
                undefined ->
                    ok;
                {_, Ref1} ->
                    erlang:demonitor(Ref1, [flush])
            end

    end,
    case del_worker(Table, Conv) of
        undefined ->
            ok;
        WorkerPID ->
            catch kcp_misc:close(WorkerPID, normal),
            case del_worker_aux(Table, WorkerPID) of
                undefined ->
                    ok;
                {_, Ref2} ->
                    erlang:demonitor(Ref2, [flush])
            end
    end,
    ok.

maybe_close_conv(DownPID) ->
    Table = ?GET_ETS_TABLE(),
    case get_agent_aux(Table, DownPID) of
        undefined ->
            case get_worker_aux(Table, DownPID) of
                undefined ->
                    ignore;
                {Conv, _} ->
                    do_close_conv(Conv)
            end;
        {Conv, _} ->
            do_close_conv(Conv)
    end.

%% 外部使用的接口
my_agent_key(PName, Conv) ->
    get_agent_key(?ETS_KCP_MANAGER(PName), Conv).

%% 注册worker pid
reg_worker(PName, Conv, WorkerPID) ->
    gen_server:call(PName, {reg_worker, Conv, WorkerPID}, infinity).

do_reg_worker(Conv, WorkerPID) ->
    Table = ?GET_ETS_TABLE(),
    case get_agent_key(Table, Conv) of
        {_AgentPID, _Key} ->
            case get_worker(Table, Conv) of
                undefined ->
                    Ref = erlang:monitor(process, WorkerPID),
                    set_worker(Table, Conv, WorkerPID),
                    set_worker_aux(Table, WorkerPID, {Conv, Ref}),
                    ok;
                WorkerPID ->
                    ok;
                OtherPID ->
                    {error, {aready_reg, OtherPID}}
            end;
        _Err ->
            {error, {no_agent_pid, _Err}}
    end.


worker_pid(PName, Conv) ->
    get_worker(?ETS_KCP_MANAGER(PName), Conv).

%% ---------------

%% 注册的代理进程
get_agent_key(Table, Conv) ->
    ?GET_VALUE(Table, {agent_key, Conv}).
set_agent_key(Table, Conv, AgentKey) ->
    ?SET_VALUE(Table, {agent_key, Conv}, AgentKey).
del_agent_key(Table, Conv) ->
    ?DEL_VALUE(Table, {agent_key, Conv}).

get_agent_aux(Table, AgentPID) ->
    ?GET_VALUE(Table, {agent_aux, AgentPID}).
set_agent_aux(Table, AgentPID, Aux) ->
    ?SET_VALUE(Table, {agent_aux, AgentPID}, Aux).
del_agent_aux(Table, AgentPID) ->
    ?DEL_VALUE(Table, {agent_aux, AgentPID}).

get_pointer(Table) ->
    ?GET_VALUE(Table, pointer, 0).
set_pointer(Table, Pointer) ->
    ?SET_VALUE(Table, pointer, Pointer).

%% 注册的Worker进程
get_worker(Table, Conv) ->
    ?GET_VALUE(Table, {worker, Conv}).
set_worker(Table, Conv, WorkerPID) ->
    ?SET_VALUE(Table, {worker, Conv}, WorkerPID).
del_worker(Table, Conv) ->
    ?DEL_VALUE(Table, {worker, Conv}).

get_worker_aux(Table, WorkerPID) ->
    ?GET_VALUE(Table, {worke_aux, WorkerPID}).
set_worker_aux(Table, WorkerPID, Aux) ->
    ?SET_VALUE(Table, {worke_aux, WorkerPID}, Aux).
del_worker_aux(Table, WorkerPID) ->
    ?DEL_VALUE(Table, {worke_aux, WorkerPID}).