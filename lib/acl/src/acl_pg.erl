-module(acl_pg).

-export([get_role/1
        ,set_role/1
        ,alloc_role/0
        ,alloc_role/1
        ,create_role/1
        ,delete_role/1
        ]).

-export([get_resource/1
        ,set_resource/1
        ,alloc_resource/0
        ,alloc_resource/1
        ,create_resource/1
        ,delete_resource/1
        ]).

-include("acl.hrl").

%% Role API
get_role(RoleId) ->
    model_kv_pg:read(RoleId, ?acl_role, acl_role).

set_role(Role) ->
    model_kv_pg:update(Role#acl_role.id, Role, ?acl_role, acl_role).

alloc_role() ->
    model_kv_pg:alloc(acl_role).

alloc_role(UUID) ->
    case model_kv_pg:exists(UUID, acl_role) of
        true  -> alloc_role();
        false ->
            model_kv_pg:create(UUID,
                               #acl_role{id = UUID, member_of = []},
                               ?acl_role,
                               acl_role),
            UUID
    end.

create_role(Role) ->
    Id = alloc_role(),
    NRole = Role#acl_role{id = Id},
    set_role(NRole),
    {ok, NRole}.

delete_role(#acl_role{id = Id}) ->
    model_kv_pg:delete(Id, acl_role).

%% Resource API
get_resource(ResourceId) ->
    model_kv_pg:read(ResourceId, ?acl_resource, acl_resource).

set_resource(#acl_resource{id = Id} = Resource) ->
    model_kv_pg:update(Id, Resource, ?acl_resource, acl_resource).

alloc_resource() ->
    model_kv_pg:alloc(acl_resource).

alloc_resource(UUID) ->
    case model_kv_pg:exists(UUID, acl_resource) of
        true  -> alloc_resource();
        false ->
            model_kv_pg:create(UUID,
                               #acl_resource{id = UUID, actions = []},
                               ?acl_resource,
                               acl_resource),
            UUID
    end.


create_resource(Resource) ->
    Id = alloc_resource(),
    NResource = Resource#acl_resource{id = Id},
    set_resource(NResource),
    NResource.

delete_resource(#acl_resource{id = Id}) ->
    model_kv_pg:delete(Id, acl_resource).

