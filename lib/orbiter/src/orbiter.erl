%%%-------------------------------------------------------------------
%%% @author Paul Khusainov <alvelcom@gmail.com>
%%% @copyright 2013 Paul Khusainov
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(orbiter).

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

-export([submit/4
        ,table/1
        ,notify/4
        ]).

-record(state, {c2p, p2c}).

%%%===================================================================
%%% API
%%%===================================================================


submit(Submission, Problem, Contest, Tag) ->
    {ok, Pid} = gen_server:call(?MODULE, {dispatch, Contest}),
    gen_server:call(Pid, {submit, Submission, Problem, Contest, Tag}).

table(Contest) ->
    {ok, Pid} = gen_server:call(?MODULE, {dispatch, Contest}),
    gen_server:call(Pid, {table, Contest}).

% for worker
notify(Submission, Problem, Contest, Verdict) ->
    {ok, Pid} = gen_server:call(?MODULE, {dispatch, Contest}),
    gen_server:call(Pid, {notify, Submission, Problem, Contest, Verdict}).


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
    C2p = ets:new(orbiter_dispatch, []),
    P2c = ets:new(orbiter_back, []),
    {ok, #state{c2p = C2p, p2c = P2c}}.

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
handle_call(stop, _, State) ->
    {stop, normal, ok, State};
handle_call({dispatch, Contest}, _, State) ->
    {reply, dispatch(Contest, State), State}.

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
handle_info({'EXIT', Pid, _}, State) ->
    case ets:lookup(State#state.p2c, Pid) of
        [{_, Contest}] ->
            ets:delete(State#state.p2c, Pid),
            ets:delete(State#state.c2p, Contest),
            dispatch(Contest, State),
            {noreply, State};
        _ ->
            {noreply, State}
    end;
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
terminate(_Reason, _State) ->
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

dispatch(Contest, S) ->
    case ets:lookup(S#state.c2p, Contest) of
        [{_, Pid}] ->
            {ok, Pid};
         _ ->
            {ok, NPid} = cont_orbiter:start_link(Contest),
            ets:insert(S#state.c2p, {Contest, NPid}),
            ets:insert(S#state.p2c, {NPid, Contest}),
            {ok, NPid}
    end.

