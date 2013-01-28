-module(poolpool).
-behaviour(gen_server).

%% API
-export([start_link/0]).

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

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Pools, Config]) ->
    process_flag(trap_exit, true),
    Active = queue:new(),
    Down   = queue:new(),
    Backup = queue:new(),
    Table  = ets:new(poolpool_wrk2boy, []),
    {ok, Sup} = poolpool_sup:start_link(?UNDERPOOL, []),
    State = #state{
            supervisor = Sup,
            active = Active,
            down   = Down,
            backup = Backup,
            config = Config,
            wrk2boy= Table
            },
    Fun = fun (Pool) -> proplists:get_bool(backup, Pool) end,
    ActiveList = lists:filter(Fun, Pools),
    BackupList = lists:filter(fun (Pool) -> not Fun(Pool) end, Pools),
    New1State = populate(ActiveList, active, State),
    New2State = populate(BackupList, backup, New1State),
    {ok, New2State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

populate([], _, State) -> State;
populate([PoolConf | Pools], active = Type, State) ->
    Pid = run_pool(PoolConf, State#state.supervisor),
    Queue = queue:in(Pid, State#state.active),
    populate(Pools, Type, State#state{active = Queue}).

run_pool({PoolConf, WorkerConf}, Sup) ->
    Pid = supervisor:start_child(Sup, poolboy:child_spec(PoolConf, WorkerConf)),
    link(Pid),
    Pid.

