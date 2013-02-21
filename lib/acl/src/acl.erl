%% @doc ACL frontend
-module(acl).

-export([ask/4
        ,update_precedents/3
        ,set_precedents/3
        ,get_precedents/2
        ,get_roles/2
        ,get_all_roles/2
        ]).
-export_type([role_id/0
             ,resource_id/0
             ,action/0
             ,actions/0
             ,verdict/0
             ,verdicts/0
             ,precedent/0
             ,precedents/0
             ]).

-include("acl.hrl").

%% Some useful types
-type action()  :: atom().
-type actions() :: action() | [action()].

-type verdict()  :: allow | deny.
-type verdicts() :: verdict() | [verdicts()].

-type precedent()  :: {role_id(), action(), verdict()}.
-type precedents() :: precedent() | [precedent()].

-type answer() :: ok | {fail, atom()} | {fail, atom(), string()}.

-type conn() :: term().

%% Underlying persistent storage
-define(u, acl_pg).

default_policy(deny)  -> deny;
default_policy(allow) -> allow;
default_policy(undefined) -> deny;
default_policy(List) when is_list(List) ->
    default_policy(default_policy_list(List)).

default_policy(deny, _) -> deny;
default_policy(_, deny) -> deny;
default_policy(allow, _) -> allow;
default_policy(_, allow) -> allow;
default_policy(_, _) -> undefined.

default_policy_list([deny|_]) -> deny;
default_policy_list([H|T]) -> default_policy(H, default_policy_list(T));
default_policy_list([]) -> undefined.

%% @doc Ask for permission
-spec ask(conn(), role_id(), actions(), resource_id()) -> verdicts().
ask(C, Role, Action, Resource) when is_atom(Action) ->
    case ask(C, Role, [Action], Resource) of
        [Ans] -> Ans;
        Error -> Error
    end;

ask(C, RoleId, Actions, ResourceId) ->
    Resource = (?u:get_resource(C, ResourceId))#acl_resource.actions,
    [default_policy(ask_(C, RoleId, Action, Resource, []))
     || Action <- Actions].

ask_(C, RoleId, Action, Resource, Visited) ->
    {Allow, Deny} = proplists:get_value(Action, Resource, {[], []}),
    IsDeny  = ordsets:is_element(RoleId, Deny),
    IsAllow = ordsets:is_element(RoleId, Allow),
    if
        IsDeny -> deny;
        IsAllow -> allow;
        true ->
            Role = ?u:get_role(C, RoleId),
            NVis = ordsets:add_element(RoleId, Visited),
            default_policy_list([
                    ask_(C, RID, Action, Resource, NVis)
                    || RID <- Role#acl_role.member_of,
                       not ordsets:is_element(RID, NVis)])
    end.

%% @doc Incrementally update precedents for the given resource
%%
%% When resource does not have {Role, Action} pair setted it creates
%% a new {Role, Action} which maps to the specified Verdict.
%%
%% When resource have {Role, Action} pair, update_precedents remaps it
%% to the specified Verdict.
-spec update_precedents(conn(), resource_id(), precedents()) -> answer().
update_precedents(C, Resource, Precedents) -> {fail, todo}.


%% @doc Fully rewrites a precedent list for the given resource
%%
%% Erases old precedent list and create a new one based on Precedents.
-spec set_precedents(conn(), resource_id(), precedents()) -> answer().
set_precedents(C, Resource, Precedents) -> {fail, todo}.


%% @doc Gets all precedents for the given resource
-spec get_precedents(conn(), resource_id()) -> precedents().
get_precedents(C, Resource) -> [].


%% @doc Gets primary roles for the given role
-spec get_roles(conn(), role_id()) -> [role_id()].
get_roles(C, RoleId) ->
    Role = ?u:get_role(C, RoleId),
    Role#acl_role.member_of.


%% @doc Gets all roles for the given role
-spec get_all_roles(conn(), role_id()) -> [role_id()].
get_all_roles(C, Role) -> get_all_roles_(C, [Role], []).

get_all_roles_(_, [], Visited) -> Visited;
get_all_roles_(C, [Current|Tail], Visited) ->
    Member = get_roles(C, Current),
    New = ordsets:subtract(Member, Visited),
    get_all_roles_(C, ordsets:union(New, Tail), ordsets:union(New, Visited)).


%% @doc Incrementally update roles for the given role
-spec update_roles(conn(), role_id(), [role_id()]) -> answer().
update_roles(C, Role, Roles) -> {fail, todo}.


%% @doc Rewrite roles for the given role
-spec set_roles(conn(), role_id(), [role_id()]) -> answer().
set_roles(C, RoleId, Roles) ->
    Role = ?u:get_role(C, RoleId),
    ?u:set_role(C, Role#acl_role{member_of = ordsets:from_list(Roles)}),
    ok.

%% to be continued...
