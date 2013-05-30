-module(resourcename).

-export([get_resource/1
        ,get_name/1
        ,add/2
        ,delete/1
        ,exists/1
        ]).

-define(u, model_kv_pg).
-define(r2n, acl_resourcename).
-define(n2r, acl_nameresource).

add(Resource, Name) ->
    ?u:create(Resource, Name, string, ?r2n),
    ?u:create(Name, Resource, uuid, ?n2r).

delete(Resource) ->
    {ok, Name} = get_name(Resource),
    ?u:delete(Resource, ?r2n),
    ?u:delete(Name, ?n2r).

get_resource(Name) ->
    ?u:read(Name, uuid, ?n2r).

get_name(Resource) ->
    ?u:read(Resource, string, ?r2n).

exists(Name) ->
    ?u:exists(Name, ?n2r).

