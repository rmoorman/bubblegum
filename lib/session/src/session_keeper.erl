-module(session_keeper).

-behaviour(gen_server).

%% API
-export([start_link/0
        ,load/1
        ,create/1
        ,save/1
        ,close/1
        ,stop/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("session.hrl").
-record(state, {h2rec}).


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

load(Token) ->
    gen_server:call(?MODULE, {load, Token}).

save(Record) ->
    gen_server:call(?MODULE, {save, Record}).

create(Record) ->
    gen_server:call(?MODULE, {create, Record}).

close(Record) ->
    gen_server:call(?MODULE, {close, Record}).

stop() ->
    gen_server:call(?MODULE, stop).

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
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    Token2Record = ets:new(h2record, [set, private, {keypos, 2}]),
    {ok, #state{h2rec = Token2Record}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec token_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({load, Token}, _, State) ->
    Reply = case ets:lookup(State#state.h2rec, Token) of
        [] -> {error, not_found};
        [R] -> {ok, R}
    end,
    {reply, Reply, State};
handle_call({save, Record}, _, State) ->
    ets:insert(State#state.h2rec, Record),
    {reply, ok, State};
handle_call({create, Record}, _, State) ->
    % FIXME
    % I really don't know about security of this statement.
    % What's about entropy? Is it enought?
    Rnd = [random:uniform(255) || _ <- lists:seq(1, 1 + 256 div 8)],
    Token = base64:encode(erlsha2:sha256(Rnd)),
    NewRecord = Record#session{token = Token},
    ets:insert(State#state.h2rec, NewRecord),
    {reply, {ok, NewRecord}, State};
handle_call({close, Record}, _, State) ->
    ets:delete(State#state.h2rec, Record#session.token),
    {reply, ok, State};
handle_call(stop, _, State) ->
    {stop, normal, ok, State}.

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

