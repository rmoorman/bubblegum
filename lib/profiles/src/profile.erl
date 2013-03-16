-module(profile).

-export([empty/1
        ,save/1
        ,save/2
        ,load/1
        ,find_by_login/1
        ,find_by_email/1
        ,create/1
        ,password/2
        ,check_password/2
        ,login/1
        ,id/1
        ,role/1
        ,role/2
        ,resource/1
        ,resource/2
        ,email/1
        ,email/2
        ,dict/1
        ,dict/2
        ,deleted/1
        ,deleted/2
        ]).

-include_lib("profiles/include/profiles.hrl").
-define(salt_size, 128 div 8). % in bytes

empty(Login) -> #profile{login = Login, dict = []}.

save(R) ->
    Old = model_kv_pg:read(R#profile.id, ?profile, profiles),
    save(Old, R).

save(#profile{id = Id, email = OEmail, login = OLogin} = _Old,
     #profile{id = Id, email = NEmail, login = NLogin} = R) ->
    if
        OEmail /= NEmail ->
            model_kv_pg:delete(OEmail, profiles_emails),
            model_kv_pg:create(OEmail, Id, uuid, profiles_emails);
        true -> ok
    end,
    if
        OLogin /= NLogin ->
            model_kv_pg:delete(OLogin, profiles_logins),
            model_kv_pg:create(OLogin, Id, uuid, profiles_logins);
        true -> ok
    end,
    model_kv_pg:update(R#profile.id, R, ?profile, profiles).

load(Id) -> model_kv_pg:read(Id, ?profile, profiles).

find_by_login(Login) -> model_kv_pg:read(Login, uuid, profiles_logins).
find_by_email(Email) -> model_kv_pg:read(Email, uuid, profiles_emails).

create(R) ->
    Id       = model_kv_pg:alloc(profiles),
    Role     = acl:alloc_role(Id),
    Resource = acl:alloc_resource(Id),
    NR = R#profile{id = Id, role = Role, resource = Resource},
    model_kv_pg:create(R#profile.login, Id, uuid, profiles_logins),
    model_kv_pg:create(R#profile.email, Id, uuid, profiles_emails),
    model_kv_pg:update(Id, NR, ?profile, profiles),
    NR.

password(Pass, R) when is_list(Pass) ->
    Salt = [random:uniform(255) || _ <- lists:seq(1, ?salt_size)],
    Hash     = base64:encode(erlsha2:sha512(Salt ++ Pass)),
    BaseSalt = base64:encode(Salt),
    R#profile{salt = BaseSalt, password = Hash}.

check_password(Pass, R) when is_list(Pass) ->
    Salt = base64:decode_to_string(R#profile.salt),
    Hash = base64:encode_to_string(erlsha2:sha512(Salt ++ Pass)),
    Hash =:= R#profile.password.

login(R) -> R#profile.login.
id(R)    -> R#profile.id.

role(R) -> R#profile.role.
role(Role, R) -> R#profile{role = Role}.

resource(R) -> R#profile.resource.
resource(Res, R) -> R#profile{resource = Res}.

email(R) -> R#profile.email.
email(Email, R) -> R#profile{email = Email}.

dict(R) -> R#profile.dict.
dict(Dict, R) -> R#profile{dict = Dict}.

deleted(R) -> R#profile.deleted =:= true.
deleted(true, R) ->R#profile{deleted = true};
deleted(false, R) -> R#profile{deleted = undefined}.

