-module(pg_pool_worker).

-behavior(poolboy_worker).
-export([start_link/1]).

start_link(Args) ->
    Hostname = proplists:get_value(hostname, Args, "127.0.0.1"),
    Username = proplists:get_value(username, Args, os:getenv("USER")),
    Password = proplists:get_value(password, Args, ""),
    {ok, _} = pgsql:connect(Hostname, Username, Password, Args).
