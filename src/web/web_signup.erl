-module(web_signup).
-compile(export_all).
-include("web.hrl").

main() -> #template{file = ?tpath "bare.html"}.

title() -> "Sign Up".

body() ->
    #panel {
        class = "container",
        body = 
        #panel{
            class = "form-signin",
            body=inner_body()}}.

%TODO add more validators
inner_body() -> 
    wf:wire(submit, login, #validate{validators = [
                #is_required{text = "Required"}]}),
    wf:wire(submit, email, #validate{validators = [
                #is_email{text = "Not a valid email address."}]}),
    [#h2{text = "Sign Up"}
    ,#flash{}
    ,#panel{id = msg, style = "display: none"}
    ,#textbox{id = login
             ,placeholder = "Login"
             ,next = email
             ,class="input-block-level"}
    ,#textbox{id = email
             ,placeholder = "Email address"
             ,next = password
             ,class="input-block-level"}
    ,#textbox{id = password
             ,placeholder = "Password"
             ,next = password2
             ,class="input-block-level"}
    ,#textbox{id = password2
             ,placeholder = "Password again"
             ,next = submit
             ,class="input-block-level"}
    ,#button{id  = submit
            ,text = "Sign Up"
            ,class = "btn btn-large btn-primary"
            ,postback = register}
    ,#link{id = signin
          ,text = "Sign In"
          ,url = "/signin"
          ,class="btn btn-large pull-right"}
    ].

bad_news(Msg) -> % bad news for you
    wf:update(msg, #panel{class = alert, body = Msg}),
    wf:wire(msg, #show{effect = "blind", speed = 500}).  

event(register) ->
    [Login, Email, Pass] = wf:mq([login, email, password]),
    Log = profile:find_by_login(Login),
    Ema = profile:find_by_email(Email),
    if
        ({error, not_found} == Log) and (Ema == Log) ->
            % ok, register
            P1 = profile:empty(Login),
            P2 = P1:email(Email),
            P3 = P2:password(Pass),
            P4 = P3:create(),
            wf:user(P4:id()),
            wf:redirect_from_login("/");
        true ->
            bad_news("Login or email is in use already.")
    end.

