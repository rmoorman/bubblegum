-module(login).
-compile(export_all).
-include("web.hrl").

main() -> #template{file = ?tpath "bare.html"}.

title() -> "Login".

body() ->
    #panel {
        class = "container",
        body = 
        #panel{
            class = "form-signin",
            body=inner_body()}}.

inner_body() -> 
    [#h2{text = "Please sign in"}
    ,#panel{id = msg, style = "display: none"}
    ,#textbox{id = username
             ,placeholder = "Email address"
             ,next = password
             ,class="input-block-level"}
    ,#textbox{id = password
             ,placeholder = "Password"
             ,next = submit
             ,class="input-block-level"}
    ,#button{id  = submit
            ,text = "Sign in"
            ,class = "btn btn-large btn-primary"
            ,postback = login}
    ].

bad_login() ->
    wf:update(msg, #panel{class = alert, body = "Email or password is invalid"}),
    wf:wire(msg, #show{effect = "blind", speed = 500}).  

event(login) ->
    [Email, Pass] = wf:mq([username, password]),
    UID = profile:find_by_email(Email),
    case UID of
        {error, _} -> bad_login(); 
        _ ->
            Profile = profile:load(UID),
            Ok = Profile:check_password(Pass),
            if
                Ok == true ->
                    wf:user(UID),
                    wf:redirect_from_login("/");
                true ->
                    bad_login()
            end
    end.

