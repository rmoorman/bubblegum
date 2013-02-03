-module(riak_helpers).
-include_lib("eunit/include/eunit.hrl").

-export([setup_riak/0, stop_riak/1]).

setup_riak() ->
    Conf = [{worker_module, riakc_wrapper},
            {size, 10},
            {max_overflow, 0}],
    {ok, Pool} = poolpool:start_link([{Conf, []}], []),
    Handle = poolpool:checkout(Pool),
    ?assert(is_pid(Handle)),
    ?assert(ok == riakc_wrapper:check_down(Handle)),
    poolpool:checkin(Pool, Handle),
    Pool.

stop_riak(Pool) ->
    poolpool:stop(Pool),
    ok.
