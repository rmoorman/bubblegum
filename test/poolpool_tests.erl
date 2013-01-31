-module(poolpool_tests).

-include_lib("eunit/include/eunit.hrl").

full_test() ->
    {ok, Pid} = poolpool:start_link([], []),
    full = poolpool:checkout(Pid),
    full = poolpool:checkout(Pid),
    full = poolpool:checkout(Pid),
    full = poolpool:checkout(Pid),
    poolpool:stop(Pid),
    ok.

one_pool_test() ->
    BoyConf    = [{worker_module, poolpool_test_worker},
                  {size, 2}, 
                  {backup, false},
                  {max_overflow, 0}],
    WorkerConf = [],
    Pool = {BoyConf, WorkerConf},
    {ok, Pid} = poolpool:start_link([Pool], []),
    One = poolpool:checkout(Pid, false),
    ?assert(is_pid(One)),
    Two = poolpool:checkout(Pid, false),
    ?assert(is_pid(Two)),
    full = poolpool:checkout(Pid, false),
    poolpool:checkin(Pid, Two),
    Two = poolpool:checkout(Pid, false),
    full = poolpool:checkout(Pid, false),
    poolpool:checkin(Pid, One),
    poolpool:checkin(Pid, Two),

    %2
    One = poolpool:checkout(Pid, false),
    ?assert(is_pid(One)),
    Two = poolpool:checkout(Pid, false),
    ?assert(is_pid(Two)),
    full = poolpool:checkout(Pid, false),
    poolpool:checkin(Pid, Two),
    Two = poolpool:checkout(Pid, false),
    full = poolpool:checkout(Pid, false),
    poolpool:checkin(Pid, One),
    poolpool:checkin(Pid, Two),

    ok.

