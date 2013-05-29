-module(m1369507051_create_roles).
-behaviour(migrator).

-export([upgrade/1
        ,downgrade/1]).

% Ok, now we should create basic backbone of the rolebased access control 
% system.
%
% This migration create roles.


upgrade(_C) -> 
    % Anonymous users
    {ok, AnonUser} = acl:alloc_role(),
    rolename:add(AnonUser, anonymous),

    % Regular users
    {ok, RegUser} = acl:alloc_role(),
    rolename:add(RegUser, user),
    acl:add_roles(RegUser, [AnonUser]),

    % Power users
    {ok, PowUser} = acl:alloc_role(),
    rolename:add(PowUser, power_user),
    acl:add_roles(PowUser, [RegUser]),

    % Admins
    {ok, Admin} = acl:alloc_role(),
    rolename:add(Admin, admin),
    acl:add_roles(Admin, [PowUser]),

    % Security admins
    {ok, SecAdmin} = acl:alloc_role(),
    rolename:add(SecAdmin, secadmin),
    acl:add_roles(SecAdmin, [Admin]),

    ok.


downgrade(_C) -> 
    List = [anonymous, user, power_user, secadmin],
    [begin
        Role = rolename:get_role(Name),
        rolename:delete(Role)
     end || Name <- List],
    ok.

