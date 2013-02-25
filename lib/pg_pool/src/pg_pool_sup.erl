-module(pg_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Env = os:getenv("PG_POOL_CONFIG"),
    ConfigName = if
        Env /= false -> Env;
        true -> "pg_pool.conf"
    end,
    {ok, _} = confetti:use(pg_pool_conf, [
                {location, {ConfigName, "conf"}},
                {subscribe, false}]),
    Conf = confetti:fetch(pg_pool_conf),
    Pool = [{name, {local, pg_pool}}, {worker_module, pg_pool_worker}]
           ++ proplists:get_value(pool, Conf, []),
    Connect = proplists:get_value(connect, Conf, []),
    PoolSpec = poolboy:child_spec(pg_pool, Pool, Connect),

    Msg = [
            {config, ConfigName}, 
            {host, proplists:get_value(hostname, Connect, "127.0.0.1")},
            {port, proplists:get_value(port, Connect, 5432)}
            ],
    error_logger:info_report(progress, Msg),

    {ok, { {one_for_one, 5, 10}, [PoolSpec]}}.

