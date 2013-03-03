-module(m1361220150_create_role_resource).
-export([upgrade/1, downgrade/1]).
-behaviour(sql_migration).

upgrade(_C) ->
    model_migrate:create_kv_table_uuid(acl_role),
    model_migrate:create_kv_table_uuid(acl_resource).

downgrade(_C) ->
    model_migrate:drop_kv_table(acl_role),
    model_migrate:drop_kv_table(acl_resource).

