-module(web_problem_edit).
-export([main/0
        ,left/0
        ,right/0
        ,title/0
        ]).

-include("web.hrl").

title() -> Problem = ?l(user), 
    case Problem:name() of
        undefinded  -> "New problem";
        V -> v
    end.

main()  -> #template{file = ?tpath "twocols.html"}.

user() ->
    case string:tokens(wf_context:path_info(), "/") of
        [Id] -> problem:load(Id);
        _    -> problem:empty()
    end.

left() ->
    [].

right() ->
    Problem = ?l(user),
    {Title, Id} = case Problem:id() of
        undefined -> {"New problem", "undefined"};
        V -> {"Editing problem \"" ++ V ++ "\"", Problem:id()}
    end,
    [#h2{text = Title}
    ,#hidden{id = id, text = Id}
    ,#panel{class = "edit", body =
        [#flash{}
        ,#p{text = "Name"}
        ,#textbox{id = name, text = Problem:name()}
        ,#p{text = "Problem statement"}
        ,#textarea{id = statement, text = Problem:body()}
        ,#p{text = "File archive"}
        ,#upload{id = file, show_button = false}
        ,#link{id = submit, class="btn btn-primary"
              ,text = "Do!"}
        ]}].

