%%%-------------------------------------------------------------------
%%% @author Paul Khusainov <alvelcom@gmail.com>
%%% @copyright 2013 Paul Khusainov
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(worker).

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

-include("worker.hrl").
-include_lib("problem_queue/include/problem_queue.hrl").

-record(state, {tag_list, id, dir}).

-define(sleep_interval, 100 * 1000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Cont} = file:read_file("conf/system_make.json"),
    Data = jsonee:decode(Cont, ?system_make),
    List = lists:usort([Cmp || {_Lang, Cmps} <- Data, {Cmp, _} <- Cmps]),
    io:format("ok, I'm worker. I found: ~p~n", [List]),
    timer:send_after(1000 * 1000, tick),
    {A, B, C} = now(),
    random:seed(A, B, C),
    T = 36#ZZZzZZZzZZZzZZZz,
    Id = integer_to_list(random:uniform(T - T div 36#z) + T div 36#z, 36),

    {ok, _} = confetti:use(worker_conf, [{location, {"worker.conf", "conf"}},
                                         {subscribe, false}]),
    Conf = confetti:fetch(worker_conf),
    Dir = proplists:get_value(workdir, Conf, "priv/worker"),
    
    filelib:ensure_dir(Dir),
    NDir = Dir ++ "/" ++ Id,
    ok = file:make_dir(NDir),
    {ok, #state{tag_list = List, id = Id, dir = NDir}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(tick, State) ->
    {noreply, tick(State)};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    os:cmd("rm -Rf " ++ State#state.dir),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%

tick(#state{tag_list = Tags} = State) ->
    case problem_queue:fetch(Tags) of
        empty ->
            timer:send_after(?sleep_interval * (80 + random:uniform(40)) div 100,
                             tick),
            State;
        {ok, Solution} ->
            State1 = performe(Solution, State),
            tick(State1)
    end.

%%
%% What should it do?
%%
%% * Prepare
%%   Well it should create temp directory,
%%   then unpack problem pack into it, copy system_make.json,
%%   then run conditer -- make conditer.
%%
%% * Check
%%   Get file from storage, rename it to solution.<ORIG_EXT>,
%%   then run judge -- make check.
%%
%% * Load results
%%   Load results.json, parse it and update a solution in 
%%   database. Then send notification to the contest's
%%   orbiter.
%%

% TODO add status messages, log infras.
performe(Solution, State) ->
    #solution{key = Id, problem = Problem
             ,contest = Contest} = Solution,
    Dir = filename:join(State#state.dir, Problem),
    NDir = binary_to_list(filename:nativename(Dir)),

    % Prepare
    case file:make_dir(NDir) of
        ok -> % directory created
            ok = prepare(Problem, NDir);
        {error, eexist} ->
            os:cmd(["rm -Rf ", NDir]),
            ok = file:make_dir(NDir),
            ok = prepare(Problem, NDir)
    end,

    % Check & load results
    ok = check(NDir),
    {ok, Results} = load_results(NDir),

    % Save to DB
    {ok, P} = submission:load(Id),
    P1 = P:verdict(Results),
    P1:save(),
           
    % Notification here (TODO)
    State.

prepare(Problem, Dir) ->
    Zip = filename:join(Dir, "problem.zip"),
    model_file:read(Problem, Zip),
    zip:extract(Zip, [{cwd, Dir}]),
    file:delete(Zip),
    run(Dir, "make conditer"),
    ok.

check(Dir) ->
    run(Dir, "make check"),
    ok.

load_results(Dir) ->
    {ok, ProblemData} = file:read_file(filename:join(Dir, "problem.json")),
    Problem = jsonee:decode(ProblemData, ?problem),

    Verdict = case file:read_file(filename:join(Dir, "results.json")) of
        {ok, Data} ->
            {ok, Json} = jsonee:decode(Data),
            Res = if
                is_list(Json) ->
                    [{Key, Value} ||
                        {{Key, _}, Value} <- lists:zip(Problem#problem.result, 
                                                       Json)];
                is_tuple(Json) ->
                    jsonee:from_eep18(Json, ?verdict)
            end;
        _ ->
            []
    end,
    {ok, Verdict}.

run(NDir, Cmd) ->
    os:cmd(["cd \"", NDir, "\"; ", Cmd]).

