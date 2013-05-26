-module(rolename).

-export([get_role/1
        ,get_name/1
        ,add/2
        ,delete/1
        ,exists/1
        ]).

-define(u, model_kv_pg).
-define(r2n, acl_rolename).
-define(n2r, acl_namerole).

-define(tostr(Q), jsonee:to_eep18(Q, string)).

add(Role, Name) ->
    ?u:create(Role, Name, string, ?r2n),
    ?u:create(?tostr(Name), Role, uuid, ?n2r).

delete(Role) ->
    {ok, Name} = get_name(Role),
    ?u:delete(Role, ?r2n),
    ?u:delete(Name, ?n2r).

get_role(Name) ->
    ?u:read(?tostr(Name), uuid, ?n2r).

get_name(Role) ->
    ?u:read(Role, string, ?r2n).

exists(Name) ->
    ?u:exists(?tostr(Name), ?n2r).

