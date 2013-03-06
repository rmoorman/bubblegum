-module(www_bare).
-compile(export_all).
-include("web.hrl").

menu() ->
    P = "pull-right",
    Profile = case wf:user() of
        undefined ->
            [["Sign In", "/signin", P]
            ,["Sign Up", "/signup", P]
            ];
        UID ->
            User = profile:load(UID),
            [["Logout", "/logout", P]
            ,[User:login(), "/profile/" ++ User:login(), P]
            ]
    end,
    [["Main page", "/", ""]
    ,["Contests", "/contests", ""]
    ,["Problems", "/problems", ""]
    ,["About", "/about", ""]
    ] ++ Profile.

navbar() ->
    Map = [item@text, item@url, lstitem@class],
    List = #bind{data = menu(), map = Map, body =
                 [#listitem{id = lstitem, body = #link{id = item}}]},
    #panel{class = "nav-collapse collapse", body = 
           #list{body = List, class = "nav"}}.


