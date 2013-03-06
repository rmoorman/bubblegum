-module (web_index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./priv/templates/bare.html" }.

title() -> "Welcome to Funtester".

body() ->
    #panel {
        class = "container",
        body = 
        #panel{
            class ="hero-unit",
            body=inner_body()}}.

inner_body() -> 
    [
        #h1{ text="Funtester" },
        #p{text = "If you can see this page, then funtester is up and running."},
        #p{text = "Click the button below to test nitrogen postbacks."},
        #button{ id=button, class="btn btn-primary btn-large", text="Click me!", postback=click }

    ].
	
event(click) ->
    wf:replace(button, #panel { 
        class="alert alert-success",
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).
