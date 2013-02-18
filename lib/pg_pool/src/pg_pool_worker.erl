-module(pg_pool_worker).

-behavior(poolboy_worker).
-export([start_link/1]).

start_link(Args) ->
    Hostname = proplists:get_value(hostname, Args),
    Database = proplists:get_value(database, Args),
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    {ok, _} = pgsql:connect(Hostname, Username, Password, [
                {database, Database}
                ]).
