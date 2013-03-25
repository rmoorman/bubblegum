-module(web_profile).
-export([main/0
        ,left/0
        ,right/0
        ,title/0
        ]).

-include("web.hrl").

title() -> User = ?l(user), User:login().
main()  -> #template{file = ?tpath "twocols.html"}.

user() ->
    [Login|_] = string:tokens(wf_context:path_info(), "/"),
    UID = profile:find_by_login(Login),
    profile:load(UID).


left() ->
    Profile = ?l(user),
    #panel{class="img-polaroid", body = 
           #gravatar{email = Profile:email(), size="210"}}.

right() ->
    Profile = ?l(user),
    D = Profile:dict(),
    [#h1{text = Profile:login()}
    ,#p{text = "Some information about this person is here."}].

format(_, undefined) -> [];
format(full_name, FullName) ->
    #span{text = FullName};
format(reg_date, Date) -> [].
