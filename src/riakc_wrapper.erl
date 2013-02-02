% Like a monkey patching
-module(riakc_wrapper).
-behaviour(poolpool).

-export([start_link/1, stop/1]).
-export([check_down/1]).

start_link(Args) ->
    Address = proplists:get_value(address, Args, "localhost"),
    Port = proplists:get_value(port, Args, 8087),
    Opts = proplists:get_value(opts, Args, []),
    riakc_pb_socket:start_link(Address, Port, Opts).

check_down(Pid) ->
    case riakc_pb_socket:ping(Pid) of
        pong -> ok;
        _    -> fail
    end.

stop(Pid) ->
    riakc_pb_socket:stop(Pid).

