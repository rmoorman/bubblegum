-module(m1369844383_add_resources).
-behaviour(migrator).

-export([upgrade/1
        ,downgrade/1
        ]).

upgrade(_C) ->
    {ok, System} = acl:alloc_resource(),
    acl:set_precedents(System, precedents:list(system, no_one)),
    ok.

downgade(_C) ->
    ok.

