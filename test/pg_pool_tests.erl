-module(pg_pool_tests).

-include_lib("eunit/include/eunit.hrl").

start() ->
    [application:start(App) || App <- [sasl, confetti, pg_pool]].


a_enviroment_set_test() ->
    start(),

    %% A-la ping postgres, check enviroment settings    
    {ok, _, [{1}]} = ppg:equery("SELECT 1", []).

