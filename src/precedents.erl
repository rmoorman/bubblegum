-module(precedents).

-export([list/2
         ]).

-define(roles, {Anon, User, Pow, Admin, SecAdmin}).
roles() ->
    {ok, Anon} = rolename:get_role(anonymous),
    {ok, User} = rolename:get_role(user),
    {ok, Pow}  = rolename:get_role(power_user),
    {ok, Admin}= rolename:get_role(admin),
    {ok, SecAdmin} = rolename:get_role(secadmin),
    ?roles.


%% list/2
-spec list(Type :: atom(), Creator :: acl:role_id()) -> 
    [{acl:role_id(), atom(), atom()}].

list(user, C) ->
    ?roles = roles(),
    [{Anon, read, allow}
    ,{C, update, allow}
    ,{Admin, update, allow}
    ,{C, delete, allow}
    ,{Admin, delete, allow}
    ];

list(system, _C) ->
    ?roles = roles(),
    [{Pow, create_contest, allow}
    ,{Admin, delegate_power, allow}
    ,{SecAdmin, delegate_admin, allow}
    ,{SecAdmin, delegate_secadm, allow}
    ].
