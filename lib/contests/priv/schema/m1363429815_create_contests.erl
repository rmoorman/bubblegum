-module(m1363429815_create_contests).
-behaviour(migrator).

-export([upgrade/1, downgrade/1]).

upgrade(_C) ->
    model_migrate:create_kv_table_uuid(contests).

downgrade(_C) ->
    model_migrate:drop_kv_table(contests).

