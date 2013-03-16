-module(m001_file_storage).
-behaviour(migrator).

-export([upgrade/1
        ,downgrade/1]).

upgrade(_C) ->
    model_migrate:create_kv_table_uuid(file_storage).

downgrade(_C) ->
    model_migrate:drop_kv_table(file_storage).

