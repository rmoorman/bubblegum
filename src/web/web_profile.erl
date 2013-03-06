-module(web_profile).
-export([main/0
        ,left/0
        ,right/0
        ]).

-include("web.hrl").

main() -> user(), #template{file = ?tpath "twocols.html"}.

user() ->
    User = get(this_user),
    if
        User == undefined ->
            [Login|_] = string:tokens(wf_context:path_info(), "/"),
            UID = profile:find_by_login(Login),
            Profile = profile:load(UID),
            put(this_user, Profile),
            Profile;
        true ->
            User
    end.


left() ->
    Profile = user(),
    #panel{class="profile-ava", body = 
           #gravatar{email = Profile:email(), size="210"}}.

right() ->
    Profile = user(),
    [#h1{text = Profile:login()}
    ,#p{text = "Some information about this person is here."}].
