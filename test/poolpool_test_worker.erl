-module(poolpool_test_worker).
-behaviour(gen_server).
-behaviour(poolpool).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([check_down/1]).
-export([cookie/1]).

%% API
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    gen_server:call(Pid, die).

cookie(Id) ->
    gen_server:call(Id, get_state).

check_down(_Id) -> ok.

%% gen_server
init(State) ->
    {ok, State}.

handle_call(die, _From, State) ->
    {stop, {error, died}, dead, State};
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_Event, _From, State) ->
    {reply, ok, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
