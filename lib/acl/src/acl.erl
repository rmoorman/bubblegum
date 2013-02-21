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

%% @doc Ask for permission
-spec ask(conn(), role_id(), actions(), resource_id()) -> verdicts().
ask(C, Role, Action, Resource) when is_atom(Action) ->
    case ask(C, Role, [Action], Resource) of
        [Ans] -> Ans;
        Error -> Error
    end;

ask(C, Role, Actions, Resource) ->
    [allow || _ <- Actions]. % haha TODO


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
get_roles(C, Role) -> [].


%% @doc Gets all roles for the given role
-spec get_all_roles(conn(), role_id()) -> [role_id()].
get_all_roles(C, Role) -> [].


%% @doc Incrementally update roles for the given role
-spec update_roles(conn(), role_id(), [role_id()]) -> answer().
update_roles(C, Role, Roles) -> {fail, todo}.


%% @doc Rewrite roles for the given role
-spec set_roles(conn(), role_id(), [role_id()]) -> answer().
set_roles(C, Role, Roles) -> {fail, todo}.

%% to be continued...
