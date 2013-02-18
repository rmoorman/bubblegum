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
    {ok, _} = confetti:use(pg_pool_conf, [
                {location, {"pg_pool.conf", "conf"}},
                {subscribe, false}]),
    Conf = confetti:fetch(pg_pool_conf),
    Pool = [{name, {local, pg_pool}}, {worker_module, pg_pool_worker}]
           ++ proplists:get_value(pool, Conf, []),
    Connect = proplists:get_value(connect, Conf, []),
    PoolSpec = poolboy:child_spec(pg_pool, Pool, Connect),

    {ok, { {one_for_one, 5, 10}, [PoolSpec]}}.

