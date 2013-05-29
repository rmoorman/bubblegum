-module(m1363429815_create_problems).
-behaviour(migrator).

-export([upgrade/1, downgrade/1]).

upgrade(_C) ->
    model_migrate:create_kv_table_uuid(problems),
    model_migrate:create_kvs_table_text(problems_tags),
    model_migrate:create_stream_table_uuid(problems_stream).

downgrade(_C) ->
    model_migrate:drop_kv_table(problems),
    model_migrate:drop_kvs_table(problems_tags),
    model_migrate:drop_stream_table(problems_stream).

