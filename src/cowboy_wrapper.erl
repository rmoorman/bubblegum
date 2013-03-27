-module(cowboy_wrapper).

-export([start/0]).

start() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/users", users_handler, []}
              ,{"/user/:user_id", users_handler, []}
              ]}]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
                {env, [{dispatch, Dispatch}]}]).


