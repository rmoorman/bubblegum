%% @doc Phantom PostgreSQL
%%
%% Just use process dictionary to store
%% postgresql connection pid.
%%
%% One of the possible improvements is to use idle timeout:
%% if there are no queries and other movements then we should
%% checkin connection to pg_pool back.
%%
%% Note that default poolboy behaviour (when client process is dieing)
%% is to automatically checkin connection pid back to pool.

-module(ppg).

-export([get_conn/0
        ,close/0
        ,equery/2
        ,squery/1
        ,renew/0
        ]).

%% Some useful defines
-define(pool, pg_pool).
-define(name, '?phantom-pgsql-pool').
-define(try_count, 3).

%% Internal
-define(pid, get(?name)).
-define(set(Pid), put(?name, Pid)).
-define(check, 
    begin
        Cond = is_pid(?pid) andalso is_process_alive(?pid),
        if
            Cond -> ok;
            true -> renew()
        end
    end).
-define(checkin,
    begin
        Pid = ?pid,
        IsPid = is_pid(Pid),
        if
           IsPid -> poolboy:checkin(pg_pool, Pid);
           true -> ok
        end
    end).

%% Interface
get_conn() ->
    ?check,
    ?pid.

close() ->
    ?checkin,
    erase(?name),
    ok.

renew() ->
    ?checkin,
    ?set(poolboy:checkout(?pool)),
    ?pid.

equery(Q, P) ->
    equery(Q, P, ?try_count).

equery(Q, P, 1) -> pgsql:equery(?pid, Q, P);
equery(Q, P, N) ->
    try pgsql:equery(?pid, Q, P)
    catch
        exit:{noproc, _Params} ->
            renew(),
            equery(Q, P, N - 1)
    end.

squery(Q) ->
    squery(Q, ?try_count).

squery(Q, 1) -> pgsql:squery(?pid, Q);
squery(Q, N) ->
    try pgsql:squery(?pid, Q)
    catch
        exit:{noproc, _Params} ->
            renew(),
            squery(Q, N - 1)
    end.

