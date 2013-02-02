-module(poolpool).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, stop/1]).
-export([checkout/1, checkout/2, checkin/2, checkin/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
        supervisor :: pid(),
        active :: queue(),
        down   :: queue(),
        backup :: queue(),
        config :: term(),
        wrk2boy:: term() %% ets table
        }).

-define(UNDERPOOL, poolboy).
-define(TRIES, 3).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Pools, Config) ->
    gen_server:start_link(?MODULE, [Pools, Config], []).

start_link([Pools, Config]) ->
    start_link(Pools, Config).

-spec checkout(Id :: pid()) -> full | pid().
checkout(Id) ->
    checkout(Id, true).

-spec checkout(Id :: pid(), Block :: boolean()) -> full | pid().
checkout(Id, Block) ->
    checkout(Id, Block, ?TRIES).

-spec checkout(Id :: pid(), Block :: boolean(), Tries :: integer()) 
    -> full | pid().
checkout(_, _, 0) -> full;
checkout(Id, Block, Tries) ->
    {PoolBoy, Ets} = gen_server:call(Id, get_pool),
    case PoolBoy of
        empty -> full;
        {value, PoolBoyPid} ->
            Worker = poolboy:checkout(PoolBoyPid, Block),
            if 
                is_pid(Worker) ->
                    ets:insert(Ets, {Worker, PoolBoyPid}),
                    Worker;
                true ->
                    checkout(Id, Block, Tries-1)
            end
    end.

-spec checkin(Id :: pid(), Worker :: pid()) -> ok.
checkin(Id, Worker) ->
    checkin(Id, Worker, active).

-spec checkin(Id :: pid(), Worker :: pid(), State :: active | down) -> ok.
checkin(Id, Worker, State) ->
    Ets = gen_server:call(Id, get_wrk2boy),
    List = ets:lookup(Ets, Worker),
    case List of
        [] -> ok; %%FIXME
        [{Worker, BoyPid}] ->
            poolboy:checkin(BoyPid, Worker),
            ets:delete(Ets, Worker),
            if 
                State == down ->
                    gen_server:cast(Id, {set_down, BoyPid});
                true -> ok
            end,
            ok
    end.

-spec stop(Id :: pid()) -> term().
stop(Id) ->
    gen_server:call(Id, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Pools, Config]) ->
    process_flag(trap_exit, true),
    Active = queue:new(),
    Down   = queue:new(),
    Backup = queue:new(),
    Table  = ets:new(poolpool_wrk2boy, [public]),
    {ok, Sup} = poolpool_sup:start_link(),
    State = #state{
            supervisor = Sup,
            active = Active,
            down   = Down,
            backup = Backup,
            config = Config,
            wrk2boy= Table
            },
    Fun = fun ({Pool, _}) -> not proplists:get_value(backup, Pool, false) end,
    ActiveList = lists:filter(Fun, Pools),
    BackupList = lists:filter(fun (Pool) -> not Fun(Pool) end, Pools),
    New1State = populate(ActiveList, active, State),
    New2State = populate(BackupList, backup, New1State),
    {ok, New2State}.

%%
handle_call(get_wrk2boy, _From, State) ->
    {reply, State#state.wrk2boy, State};
handle_call(get_pool, _From, State) ->
    #state{
        active = Active,
        backup = Backup,
        wrk2boy = EtsId
        } = State,
    {ActPid, NewActive} = round_robin(Active),
    {BoyPid, NewBack} = if
        ActPid == empty ->
            round_robin(Backup);
        true ->
            {ActPid, Backup}
    end,
    {reply, {BoyPid, EtsId}, State#state{active = NewActive, backup = NewBack}};

handle_call(get_down_pool, _From, State) ->
    {Ans, NewDown} = round_robin(State#state.down),
    {reply, Ans, State#state{down = NewDown}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%
handle_cast({set_down, Pool}, State) ->
    Fun = fun (E) -> Pool /= E end,
    IsMember = queue:member(Pool, State#state.active),
    if
         IsMember ->
            NewActive = queue:filter(Fun, State#state.active),
            NewDown   = queue:in(Pool, State#state.down),
            {noreply, State#state{active = NewActive, down = NewDown}};
        true ->
            {noreply, State}
    end;

handle_cast({set_active, Pool}, State) ->
    Fun = fun (E) -> Pool /= E end,
    IsMember = queue:member(Pool, State#state.down),
    if
         IsMember ->
            NewDown   = queue:filter(Fun, State#state.down),
            NewActive = queue:in(Pool, State#state.active),
            {noreply, State#state{active = NewActive, down = NewDown}};
        true ->
            {noreply, State}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:delete(State#state.wrk2boy),
    true = exit(State#state.supervisor, shutdown),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
populate(Pools, Type, State) ->
    populate(Pools, 0, Type, State).

populate([], _, _, State) -> State;
populate([PoolConf | Pools], Num, active = Type, State) ->
    Pid = run_pool({Num, Type}, PoolConf, State#state.supervisor),
    Queue = queue:in(Pid, State#state.active),
    populate(Pools, Num+1, Type, State#state{active = Queue}).

run_pool(Name, {PoolConf, WorkerConf}, Sup) ->
    Arg = poolboy:child_spec(Name, PoolConf, WorkerConf),
    {ok, Pid} = supervisor:start_child(Sup, Arg),
    link(Pid),
    Pid.

round_robin(Queue) ->
    {Ans, Q1} = queue:out(Queue),
    if
        Ans == empty ->
            {empty, Queue};
        true ->
           {value, Item} = Ans, 
            Q2 = queue:in(Item, Q1),
            {Ans, Q2}
    end.


%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

round_robin_test() ->
    Emt = queue:new(),
    ?assert({empty, Emt} == round_robin(Emt)),
    Oneel = queue:in(1, Emt),
    {{value, 1}, Q1} = R1 = round_robin(Oneel),
    {{value, 1}, Q2} = R2 = round_robin(Q1),
    FullQueue = lists:foldl(fun (El, Acc) ->
                queue:in(El, Acc)
        end, Q2, lists:seq(2, 100)),

    S = lists:seq(1, 100),
    lists:foldl(fun (El, Acc) ->
                io:format("Next item is ~B~n", [El]),
                {{value, El}, Out} = round_robin(Acc),
                Out
        end, FullQueue, S ++ S ++ S),
    ok.
    
set_down_test() ->
    Emt = queue:new(),
    List = lists:seq(1, 100),
    Queue = queue:from_list(List),
    State = #state{
            active = Queue,
            down = Emt,
            backup = Emt
            },
    {noreply, State} = handle_cast({set_down, 4242}, State),
    {noreply, State1} = handle_cast({set_down, 42}, State),
    ?assert(false =:= queue:member(42, State1#state.active)),
    ?assert(true  =:= queue:member(42, State1#state.down)),
    {noreply, State1} = handle_cast({set_down, 42}, State1),
    ok.

get_pool_test() ->
    Emt = queue:new(),
    Active = queue:from_list([1, 2, 3, 4]),
    Backup = queue:from_list([5, 6, 7]),
    StateA = #state{
            active = Active,
            down   = Emt,
            backup = Emt
            },
    {reply, Ans1, StateA1} = handle_call(get_pool, undefined, StateA),
    {{value, 1}, _} = Ans1,
    {reply, Ans2, StateA2} = handle_call(get_pool, undefined, StateA1),
    {{value, 2}, _} = Ans2,
    {reply, Ans3, StateA3} = handle_call(get_pool, undefined, StateA2),
    {{value, 3}, _} = Ans3,
    {reply, Ans4, StateA4} = handle_call(get_pool, undefined, StateA3),
    {{value, 4}, _} = Ans4,
    {reply, Ans5, StateA5} = handle_call(get_pool, undefined, StateA4),
    {{value, 1}, _} = Ans5,

    StateB = #state{
            active = Emt,
            down = Active,
            backup = Backup
            },
    {reply, AnsB1, StateB1} = handle_call(get_pool, undefined, StateB),
    {{value, 5}, _} = AnsB1,
    {reply, AnsB2, StateB2} = handle_call(get_pool, undefined, StateB1),
    {{value, 6}, _} = AnsB2,
    {reply, AnsB3, StateB3} = handle_call(get_pool, undefined, StateB2),
    {{value, 7}, _} = AnsB3,
    {reply, AnsB4, StateB4} = handle_call(get_pool, undefined, StateB3),
    {{value, 5}, _} = AnsB4,
    
    StateC = #state{
            active = Emt,
            down = Active,
            backup = Emt
            },
    {reply, AnsC1, StateC1} = handle_call(get_pool, undefined, StateC),
    {empty, _} = AnsC1,
    {reply, AnsC2, StateC2} = handle_call(get_pool, undefined, StateC1),
    {empty, _} = AnsC2,
    {reply, AnsC3, StateC3} = handle_call(get_pool, undefined, StateC2),
    {empty, _} = AnsC3,

    ok.


-endif.

