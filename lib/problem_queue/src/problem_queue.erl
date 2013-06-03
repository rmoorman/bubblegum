%%%-------------------------------------------------------------------
%%% @author Paul Khusainov <alvelcom@gmail.com>
%%% @copyright 2013 Paul Khusainov
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(problem_queue).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([add/4,
         fetch/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("problem_queue.hrl").

-record(state, {table}).

%%%===================================================================
%%% API
%%%===================================================================

add(Key, Problem, Contest, Tag) ->
    Sol = #solution{ts = now(), key = Key, problem = Problem
                   ,contest = Contest, tag = Tag},
    gen_server:call(?MODULE, {add, Sol}).

fetch(ListOfPosibleTags) ->
    gen_server:call(?MODULE, {fetch, lists:usort(ListOfPosibleTags)}).

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
    Ets = ets:new(problems, [ordered_set, {keypos, 2}]),
    {ok, #state{table = Ets}}.

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
handle_call({add, Res}, _From, State) ->
    ets:insert_new(State#state.table, Res),
    {reply, ok, State};
handle_call({fetch, List}, _From, State) ->
    QH = qlc:q([A || #solution{tag = Tag} = A<- ets:table(State#state.table),
                      ordsets:is_element(Tag, List)]),
    C = qlc:cursor(QH),
    Ans = case qlc:next_answers(C, 1) of
        [A] -> 
            io:format("~p~n", [A]),
            ets:delete_object(State#state.table, A),
            {ok, A};
        _ ->
            empty
    end,
    ok = qlc:delete_cursor(C),
    {reply, Ans, State};
    


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
