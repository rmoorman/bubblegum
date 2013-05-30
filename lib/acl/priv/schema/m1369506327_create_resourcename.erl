-module(m1369506327_create_resourcename).
-behaviour(migrator).

-export([upgrade/1
        ,downgrade/1]).

upgrade(_C) -> 
    model_migrate:create_kv_table_uuid(acl_resourcename),
    model_migrate:create_kv_table_string(acl_nameresource).

downgrade(_C) ->
    model_migrate:drop_kv_table(acl_resourcename),
    model_migrate:drop_kv_table(acl_nameresource).

