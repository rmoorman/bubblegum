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

sim_two_poolpool_test() ->
    BoyConf = config(1),
    {ok, Pid1} = poolpool:start_link([{BoyConf, first}], []),
    {ok, Pid2} = poolpool:start_link([{BoyConf, second}], []),

    One = out(Pid1),
    ?assert(is_pid(One)),
    Two = out(Pid2),
    ?assert(is_pid(Two)),

    ?assert(first  == poolpool_test_worker:cookie(One)),
    ?assert(second == poolpool_test_worker:cookie(Two)),

    ?assert(full == out(Pid1)),
    ?assert(full == out(Pid2)),

    in(Pid1, Two),
    ?assert(full == out(Pid1)),
    ?assert(full == out(Pid2)),

    in(Pid2, One),
    ?assert(full == out(Pid1)),
    ?assert(full == out(Pid2)),

    in(Pid1, One),
    Three = out(Pid1),
    ?assert(is_pid(Three)),
    ?assert(first  == poolpool_test_worker:cookie(Three)),

    in(Pid2, Three),
    ?assert(full == out(Pid1)),
    ?assert(full == out(Pid2)),

    in(Pid1, Three),

    poolpool:stop(Pid1),
    poolpool:stop(Pid2),
    ok.


two_pool_test() ->
    BoyConf = config(1),
    Pools = [{BoyConf, one}, {BoyConf, two}],
    {ok, Pid} = poolpool:start_link(Pools, []),
    ?assert(is_pid(Pid)),

    One = out(Pid),
    Two = out(Pid),
    ?assert(is_pid(One)),
    ?assert(is_pid(Two)),

    ?assert(full == out(Pid)),
    ?assert(full == out(Pid)),

    in(Pid, One),
    Three = out(Pid),
    ?assert(is_pid(Three)),

    ?assert(full == out(Pid)),
    ?assert(full == out(Pid)),

    in(Pid, Two),
    in(Pid, Three),
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

