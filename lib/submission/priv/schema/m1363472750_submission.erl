-module(m1363472750_submission).
-behaviour(migrator).

-export([upgrade/1, downgrade/1]).

upgrade(_C) ->
    model_migrate:create_kv_table_uuid(submissions),
    model_migrate:create_kv_table_uuid(submissions_log).

downgrade(_C) ->
    model_migrate:drop_kv_table(submissions),
    model_migrate:drop_kv_table(submissions_log).

