%%%-------------------------------------------------------------------
%%% @author Paul Khusainov <alvelcom@gmail.com>
%%% @copyright 2013 Paul Khusainov
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cont_orbiter).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("model_tools/include/model.hrl").

-record(state, {contest, js, table = [], toadd = []}).

-record(item, {who
              ,problem
              ,verdict = []
              ,penalty = 0
              }).
-define(item, ?jsonee(item, [{who, uuid}
                            ,{problem, uuid}
                            ,{verdict, {dict, string, id}}
                            ,{penalty, integer}
                            ])).

-record(table_item, {id
                    ,problems
                    ,results
                    }).
-define(table_item, ?jsonee(table_item, [{id, uuid}
                                        ,{problems, {dict, string, id}}
                                        ,{results, {dict, string, id}}
                                        ])).




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
start_link(Contest) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Contest, []).

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
init(Contest) ->
    {ok, JS} = js_driver:new(),
    Dir = code:priv_dir(orbiter),
    Ans = js_driver:define_js(JS, {file, filename:join(Dir, "code.js")}),
    io:format("Load code.js from dir ~p~nJS driver said: ~p~n", [Dir, Ans]),
    print_logs(JS),
    ok = Ans,

    {ok, C} = contest:load(Contest),

    Ans2 = js_driver:define_js(JS, list_to_binary(C:code())),
    io:format("Load code from contest ~p~nJS driver said: ~p~n", 
              [Contest, Ans2]),
    print_logs(JS),
    timer:send_after(3000, tick),
    {ok, #state{contest = Contest, js = JS}}.

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
%% S -- Submission Id
%% P -- Problem Id
%% C -- Contest Id
%% T -- Tag
%% V -- Verdict
%%
handle_call({submit, S, P, C, T}, _From, State) ->
    problem_queue:add(S, P, C, T),
    {reply, ok, State};
handle_call({table, _}, _From, State) ->
    {reply, State#state.table, State};
handle_call({notify, S, P, _, V}, _From, State) ->
    {ok, Subm} = submission:load(S),
    Item = #item{who = Subm:user(), problem = P, verdict = V},
    {reply, ok, State#state{toadd = [Item | State#state.toadd]}};
handle_call(stop, _, State) ->
    {stop, normal, ok, State};
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
    print_logs(State#state.js),
    js_driver:destroy(State#state.js),
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

print_logs(JS) ->
    {ok, Logs} = js:call(JS, <<"__GetLog">>, []),
    [io:format("JS: ~s~n", [L]) || L <- Logs].


tick(#state{toadd = []} = State) ->
    timer:send_after(3000, tick),
    State;
tick(#state{toadd = Toadd, js = JS} = State) ->
    JSON = jsonee:to_mochi(Toadd, [?item]),
    Ans = js:call(JS, <<"__Add">>, [JSON]),
    print_logs(JS),
    io:format("JS said: ~p~n", [Ans]),
    {ok, TableJson} = Ans,
    Table = jsonee:from_mochi(TableJson, [?table_item]),
    timer:send_after(1000, tick),
    State#state{toadd = [], table = Table}.

