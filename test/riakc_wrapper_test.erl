-module(riakc_wrapper_test).
-include_lib("eunit/include/eunit.hrl").
-include("poolpool.hrl").

start_test() ->
    Pool = riak_helpers:setup_riak(),
    poolpool:stop(Pool),
    ok.


