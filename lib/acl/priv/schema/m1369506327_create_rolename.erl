-module(m1369506327_create_rolename).
-behaviour(migrator).

-export([upgrade/1
        ,downgrade/1]).

upgrade(_C) -> 
    model_migrate:create_kv_table_uuid(acl_rolename),
    model_migrate:create_kv_table_string(acl_namerole).

downgrade(_C) ->
    model_migrate:drop_kv_table(acl_rolename),
    model_migrate:drop_kv_table(acl_namerole).

