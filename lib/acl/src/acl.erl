%% @doc ACL frontend
-module(acl).

-export([ask/3, update_precedents/2, set_precedents/2,get_precedents/1]).
-export_type([role_id/0, resource_id/0, action/0,
              actions/0, verdict/0, verdicts/0,
              precedent/0, precedents/0]).

-type role_id() :: string().
-type resource_id() :: string().

-type action()  :: atom().
-type actions() :: action() | [action()].

-type verdict()  :: allow | deny.
-type verdicts() :: verdict() | [verdicts()].

-type precedent()  :: {role_id(), action(), verdict()}.
-type precedents() :: precedent() | [precedent()].

-type answer() :: ok | {fail, atom()} | {fail, atom(), string()}.

%% @doc Ask for permission
-spec ask(role_id(), actions(), resource_id()) -> verdicts().
ask(Role, Action, Resource) when is_atom(Action) ->
    case ask(Role, [Action], Resource) of
        [Ans] -> Ans;
        Error -> Error
    end;

ask(Role, Actions, Resource) ->
    [allow || _ <- Actions]. % haha TODO


%% @doc Incrementally update precedents for the given resource
%%
%% When resource does not have {Role, Action} pair setted it creates
%% a new {Role, Action} which maps to the specified Verdict.
%%
%% When resource have {Role, Action} pair, update_precedents remaps it
%% to the specified Verdict.
-spec update_precedents(resource_id(), precedents()) -> answer().
update_precedents(Resource, Precedents) -> {fail, todo}.


%% @doc Fully rewrites a precedent list for the given resource
%%
%% Erases old precedent list and create a new one based on Precedents.
-spec set_precedents(resource_id(), precedents()) -> answer().
set_precedents(Resource, Precedents) -> {fail, todo}.


%% @doc Gets all precedents for the given resource
-spec get_precedents(resource_id()) -> precedents().
get_precedents(Resource) -> [].


%% @doc Gets primary roles for the given role
-spec get_roles(role_id()) -> [role_id()].
get_roles(Role) -> [].


%% @doc Gets all roles for the given role
-spec get_all_roles(role_id()) -> [role_id()].
get_all_roles(Role) -> [].


%% @doc Incrementally update roles for the given role
-spec update_roles(role_id(), [role_id()]) -> answer().
update_roles(Role, Roles) -> {fail, todo}.


%% @doc Rewrite roles for the given role
-spec set_roles(role_id(), [role_id()]) -> answer().
set_roles(Role, Roles) -> {fail, todo}.

%% to be continued...
