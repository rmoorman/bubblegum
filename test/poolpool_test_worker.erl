-module(poolpool_test_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([check_down/1]).

start_link(_Args) ->
    gen_server:start_link(?MODULE, [], []).

check_down(_Id) -> ok.

init([]) ->
    {ok, undefined}.

handle_call(die, _From, State) ->
    {stop, {error, died}, dead, State};
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