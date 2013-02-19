-module(acl_pg).

-export([get_role/2, set_role/2, alloc_role/1, create_role/2, delete_role/2]).
-export([get_resource/2, set_resource/2, alloc_resource/1, create_resource/2, delete_resource/2]).

-include("acl.hrl").

%% Role API
get_role(Conn, RoleId) ->
    Query = "SELECT role_val FROM acl_role WHERE role_key = $1",
    {ok, _, Row} = pgsql:equery(Conn, Query, [RoleId]),
    [{Txt}] = Row,
    json_to_role(Txt).

set_role(Conn, Role) ->
    RoleId = Role#acl_role.id,
    Json = role_to_json(Role),
    Query = "UPDATE acl_role SET role_val = $2 WHERE role_key = $1",
    {ok, _} = pgsql:equery(Conn, Query, [RoleId, Json]),
    ok.

alloc_role(Conn) ->
    Query = "INSERT INTO acl_role (role_val) VALUES (NULL) RETURNING role_key",
    {ok, _, _, [{Key}]} = pgsql:equery(Conn, Query),
    Key.

create_role(Conn, Role) ->
    Id = alloc_role(Conn),
    NRole = Role#acl_role{id = Id},
    set_role(Conn, NRole),
    NRole.

delete_role(Conn, #acl_role{id = Id}) ->
    Query = "DELETE FROM acl_role WHERE role_key = $1",
    pgsql:equery(Conn, Query, [Id]),
    ok.

%% Resource API
get_resource(Conn, ResourceId) ->
    Query = "SELECT resource_val FROM acl_resource WHERE resource_key = $1",
    {ok, _, Row} = pgsql:equery(Conn, Query, [ResourceId]),
    [{Txt}] = Row,
    json_to_resource(Txt).

set_resource(Conn, Resource) ->
    ResourceId = Resource#acl_resource.id,
    Json = resource_to_json(Resource),
    Query = "UPDATE acl_resource SET resource_val = $2 WHERE resource_key = $1",
    {ok, _} = pgsql:equery(Conn, Query, [ResourceId, Json]),
    ok.

alloc_resource(Conn) ->
    Query = "INSERT INTO acl_resource (resource_val) VALUES (NULL) RETURNING resource_key",
    {ok, _, _, [{Key}]} = pgsql:equery(Conn, Query),
    Key.

create_resource(Conn, Resource) ->
    Id = alloc_resource(Conn),
    NResource = Resource#acl_resource{id = Id},
    set_resource(Conn, NResource),
    NResource.

delete_resource(Conn, #acl_resource{id = Id}) ->
    Query = "DELETE FROM acl_resource WHERE resource_key = $1",
    pgsql:equery(Conn, Query, [Id]),
    ok.

%% internal
detuple({A}) -> A.
roleid_to_json(Role) when is_integer(Role) -> Role;
roleid_to_json(Role) when is_binary(Role) -> Role;
roleid_to_json(Role) when is_atom(Role) -> Role;
roleid_to_json(Role) when is_list(Role) -> list_to_binary(Role).
resourceid_to_json(Resource) -> roleid_to_json(Resource).
action_to_json(Action) -> roleid_to_json(Action).

json_to_role(Json) ->
    {Decoded} = jiffy:decode(Json),
    #acl_role{
        id = proplists:get_value(<<"id">>, Decoded),
        updated_at = proplists:get_value(<<"updated_at">>, Decoded),
        updated_by = proplists:get_value(<<"updated_by">>, Decoded),
        member_of = proplists:get_value(<<"member_of">>, Decoded)
        }.

role_to_json(#acl_role{
                id = Id,
                updated_by = By,
                updated_at = At,
                member_of = Memb}) ->
    Members = lists:flatmap(fun(Arg) ->
                    [roleid_to_json(Arg)]
            end, Memb),
    jiffy:encode({[
                {<<"id">>, roleid_to_json(Id)},
                {<<"updated_by">>, roleid_to_json(By)},
                {<<"updated_at">>, At},
                {<<"member_of">>, Members}
                ]}).

json_to_resource(Json) ->
    F = fun ({Name, {D}}) ->
            {Name,
             proplists:get_value(<<"allow">>, D, []),
             proplists:get_value(<<"deny">>, D, [])}
    end,
    {Decoded} = jiffy:decode(Json),
    #acl_resource{
        id = proplists:get_value(<<"id">>, Decoded),
        updated_at = proplists:get_value(<<"updated_at">>, Decoded),
        updated_by = proplists:get_value(<<"updated_by">>, Decoded),
        actions = lists:map(F, detuple(
                proplists:get_value(<<"actions">>, Decoded)
                ))
        }.

resource_to_json(Res) ->
    #acl_resource{
        id = Id,
        updated_at = At,
        updated_by = By,
        actions = Act
        } = Res,
    JAct = lists:map(fun ({K, Allow, Deny}) ->
                    {action_to_json(K), {
                            if Allow == [] -> [];
                                true -> [{allow, Allow}]
                            end
                            ++
                            if Deny == [] -> [];
                                true -> [{deny, Deny}]
                            end
                            }}
            end, Act),
    JFill = lists:filter(fun ({_,{[]}}) -> false; (_) -> true end,
                         JAct),
    Ans = [
            {id, resourceid_to_json(Id)},
            {updated_at, roleid_to_json(At)},
            {updated_by, By},
            {actions, {JFill}}
            ],
    jiffy:encode({Ans}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

role_to_json_test() ->
    OriginJson =
                 <<"{\"id\":1,\"updated_by\":1,\"updated_at\":10000,\"member_of\":[1,2,3]}">>,
    OriginRole = #acl_role{
            id = 1,
            updated_at = 10000,
            updated_by = 1,
            member_of = [1, 2, 3]
            },
    Role = json_to_role(OriginJson),
    ?assertEqual(Role, OriginRole),
    Json = role_to_json(Role),
    Role2 = json_to_role(Json),
    ?assertEqual(Role2, OriginRole),
    ok.

-endif.
