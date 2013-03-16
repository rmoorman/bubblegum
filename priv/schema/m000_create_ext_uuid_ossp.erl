-module(m000_create_ext_uuid_ossp).
-behaviour(migrator).

-export([upgrade/1
        ,downgrade/1]).

upgrade(C) -> model_migrate:migration("CREATE EXTENSION \"uuid-ossp\"").
downgrade(C) -> model_migrate:migration("DROP EXTENSION IF EXISTS \"uuid-ossp\"").
