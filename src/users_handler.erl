-module(users_handler).
-include_lib("profiles/include/profiles.hrl").
-export([init/3
        ,allowed_methods/2
        ,content_types_provided/2
        ,content_types_accepted/2
        ,resource_exists/2
        ,post_is_create/2
        ,create_path/2
        ,to_json/2
        ,create_user/2
        ]).

init(_, _, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(R, S) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>], R, S}.

content_types_provided(R, S) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}
     ], R, S}.

content_types_accepted(R, S) ->
    {[{{<<"application">>, <<"json">>, []}, create_user}
     ], R, S}.

resource_exists(R, _S) ->
    case cowboy_req:binding(user_id, R) of
        {undefined, Req2} ->
            {true, Req2, index};
        {User, Req2} ->
            case profile:load(User) of
                {error, _} ->
                    {false, Req2, {id, User}};
                {ok, Profile} ->
                    {true, Req2, {profile, Profile}}
            end
    end.

post_is_create(R, S) -> {true, R, S}.

create_path(R, _S) ->
    Uuid = profile:alloc(),
    {<<"/user/", Uuid/binary>>, R, {id, Uuid}}.

to_json(R, {profile, Profile} = S) ->
    {Profile:to_json(), R, S}.

create_user(R, {id, Id}) ->
    {ok, Body, R2} = cowboy_req:body(R),
    Profile = profile:from_json(Body),
    Profile2 = Profile:hash_password(),
    Profile3 = Profile2:id(Id),
    ProfileE = Profile3:save(),

    {ok, UserRole} = rolename:get_role(user),
    acl:add_roles(ProfileE:role(), [UserRole]),
    {true, R2, {profile, ProfileE}};

create_user(R, {profile, OldProfile}) ->
    {ok, Body, R2} = cowboy_req:body(R),
    Profile = profile:from_json(Body),
    NewProfile = OldProfile:dict(Profile:dict()),
    NewProfile:save(),
    {true, R2, {profile, NewProfile}}.

