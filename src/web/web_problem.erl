-module(web_problem).
-export([main/0
        ,left/0
        ,right/0
        ,title/0
        ]).

-include("web.hrl").

title() -> Problem = ?l(problem), Problem:name().
main()  -> #template{file = ?tpath "twocols.html"}.

problem() ->
    [PID|_] = string:tokens(wf_context:path_info(), "/"),
    problem:load(PID).


left() ->
    Problem = ?l(problem),
    [#panel{style = "height: 55px"}
    ,#h6{text = "Tags: "}
    ,[#link{body = #label{text = Name}, url = ?url2(tag, Name)}
         || Name <- Problem:tags()]
    ].

right() ->
    Problem = ?l(problem),
    [#h2{text = Problem:name()}
    ,#panel{class = "problem-body"
           ,body = Problem:body()}].
