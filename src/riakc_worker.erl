% Like a monkey patching
-module(riakc_worker).
-behaviour(poolboy_worker).

-export([start_link/1, stop/1]).

start_link(Args) ->
    Address = proplists:get_value(address, Args, "localhost"),
    Port = proplists:get_value(port, Args, 8087),
    Opts = proplists:get_value(opts, Args, []),
    riakc_pb_socket:start_link(Address, Port, Opts).

stop(Pid) ->
    riakc_pb_socket:stop(Pid).

