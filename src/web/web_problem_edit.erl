-module(web_problem_edit).
-export([main/0
        ,left/0
        ,right/0
        ,title/0
        ,start_upload_event/1
        ,finish_upload_event/4
        ,event/1
        ]).

-include("web.hrl").

title() -> Problem = ?l(problem), 
    case Problem:name() of
        undefinded  -> "New problem";
        V -> v
    end.

main()  -> #template{file = ?tpath "twocols.html"}.

problem() ->
    case string:tokens(wf_context:path_info(), "/") of
        [Id] -> problem:load(Id);
        _    -> problem:empty()
    end.

left() ->
    [].

right() ->
    Problem = ?l(problem),
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
        ,#p{text = "Tags"}
        ,#textbox{id = tags, text = string:join(Problem:tags(), ", ")}
        ,#p{text = "File archive"}
        ,#upload{id = file, show_button = false, tag = problemUpload}
        ,#link{id = submit, class="btn btn-primary"
              ,text = "Apply", postback = do}
        ]}].

start_upload_event(problemUpload) ->
    wf:flash("Upload started").

finish_upload_event(_, undefined, _, _) ->
    wf:flash("Please select a file.");

finish_upload_event(problemUpload, FileName, LocalFile, Node) ->
    Key = model_file:create(LocalFile),
    file:delete(LocalFile),
    wf:state(fileup, Key).

event(do) ->
    P0 = ?l(problem),
    P1 = P0:name(wf:q(name)),
    P2 = P1:body(wf:q(statement)),
    P3 = P2:file(wf:state_default(fileup, P2:file())),
    Tags = [string:strip(S) || S <- string:tokens(wf:q(tags), ",")],
    P4 = P3:tags(Tags),
    Po = case P4:id() of
        undefined ->
            P4:create();
        _ ->
            P4:save()
    end,
    wf:redirect(?url2(problem, Po:id())).

