-module(acl_pg).

-include("acl.hrl").

%% internal
detuple({A}) -> A.
roleid_to_json(Role) when is_integer(Role) -> Role;
roleid_to_json(Role) when is_binary(Role) -> Role;
roleid_to_json(Role) when is_atom(Role) -> Role;
roleid_to_json(Role) when is_list(Role) -> list_to_binary(Role).
action_to_json(Action) -> roleid_to_json(Action).

json_to_role(Json) ->
    {Decoded} = jiffy:decode(Json),
    #acl_role{
        updated_at = proplists:get_value(<<"updated_at">>, Decoded),
        updated_by = proplists:get_value(<<"updated_by">>, Decoded),
        member_of = proplists:get_value(<<"member_of">>, Decoded)
        }.

role_to_json(#acl_role{
                updated_by = By,
                updated_at = At,
                member_of = Memb}) ->
    Members = lists:flatmap(fun(Arg) ->
                    [roleid_to_json(Arg)]
            end, Memb),
    jiffy:encode({[
                {<<"updated_by">>, roleid_to_json(By)},
                {<<"updated_at">>, At},
                {<<"member_of">>, Members}
                ]}).

json_to_resource(Json) ->
    F = fun (D) ->
            {proplists:get_value(<<"allow">>, D, []),
             proplists:get_value(<<"deny">>, D, [])}
    end,
    {Decoded} = jiffy:decode(Json),
    #acl_resource{
        updated_at = proplists:get_value(<<"updated_at">>, Decoded),
        updated_by = proplists:get_value(<<"updated_by">>, Decoded),
        actions = lists:map(F, detuple(
                proplists:get_value(<<"actions">>, Decoded)
                ))
        }.

resource_to_json(Res) ->
    #acl_resource{
        updated_at = At,
        updated_by = By,
        actions = Act
        } = Res,
    JAct = lists:map(fun ({K, {Allow, Deny}}) ->
                    {action_to_json(K), {[
                                {allow, Allow},
                                {deny, Deny}
                                ]}}
            end, Act),
    Ans = [
            {updated_at, roleid_to_json(At)},
            {updated_by, By},
            {actions, {JAct}}
            ],
    jiffy:encode(Ans).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

role_to_json_test() ->
    OriginJson =
                 <<"{\"updated_by\":1,\"updated_at\":10000,\"member_of\":[1,2,3]}">>,
    OriginRole = #acl_role{
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
