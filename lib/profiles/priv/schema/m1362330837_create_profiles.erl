-module(m1362330837_create_profiles).
-export([upgrade/1, downgrade/1]).
-behaviour(migrator).

upgrade(_C) ->
    model_migrate:create_kv_table_uuid(profiles),
    model_migrate:create_kv_table_string(profiles_emails),
    model_migrate:create_kv_table_string(profiles_logins).

downgrade(_C) ->
    model_migrate:drop_kv_table(profiles),
    model_migrate:drop_kv_table(profiles_emails),
    model_migrate:drop_kv_table(profiles_logins).

