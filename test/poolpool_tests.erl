-module(poolpool_tests).

-include_lib("eunit/include/eunit.hrl").

full_test() ->
    {ok, Pid} = poolpool:start_link([], []),
    full = out(Pid),
    full = out(Pid),
    full = out(Pid),
    full = out(Pid),
    poolpool:stop(Pid),
    ok.

one_pool_test() ->
    BoyConf = config(2),
    WorkerConf = [],
    Pool = {BoyConf, WorkerConf},
    {ok, Pid} = poolpool:start_link([Pool], []),
    One = out(Pid),
    ?assert(is_pid(One)),
    Two = out(Pid),
    ?assert(is_pid(Two)),
    full = out(Pid),
    in(Pid, Two),
    Two = out(Pid),
    full = out(Pid),
    in(Pid, One),
    in(Pid, Two),

    %2
    One = out(Pid),
    ?assert(is_pid(One)),
    Two = out(Pid),
    ?assert(is_pid(Two)),
    full = out(Pid),
    in(Pid, Two),
    Two = out(Pid),
    full = out(Pid),
    in(Pid, One),
    in(Pid, Two),

    poolpool:stop(Pid),
    ok.

out(Poolpool) ->
    poolpool:checkout(Poolpool, false).

in(Poolpool, Worker) ->
    poolpool:checkin(Poolpool, Worker).

config(Size) ->
    [{worker_module, poolpool_test_worker},
     {size, Size}, 
     {backup, false},
     {max_overflow, 0}].

