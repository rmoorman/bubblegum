
-include_lib("nitrogen_core/include/wf.hrl").

%% Path where templates are placed
-define(tpath, "priv/templates/").

%% Path for document root
-define(doc_root, "priv/webroot").


%% Define routing table
-define(routes, 
        [% Modules
         {"/",          web_index}

        ,{"/signin",    web_signin}
        ,{"/signup",    web_signup}
        ,{"/logout",    web_logout}

        ,{"/profile",   web_profile}
        ,{"/profiles",  web_profiles}

        ,{"/contest",   web_contest}
        ,{"/contests",  web_contests}

        ,{"/problem",   web_problem}
        ,{"/problem/edit", web_problem_edit}
        ,{"/problems/new", web_problem_edit}
        ,{"/problems",  web_problems}

         % Some static
        ,{"/bootstrap", static_file}
        ,{"/nitrogen",  static_file}
        ,{"/js",        static_file}
        ,{"/css",       static_file}
        ,{"/images",    static_file}
        ]).


%% Lazy load with process dictionary as storage
-define(l(Function), case get({lazy, Function}) of
        undefined -> 
            Value = Function(),
            put({lazy, Function}, {ok, Value}),
            Value;
        {ok, Value} ->
            Value
    end).
                
%% Bad url generator
-define(url1(Mod), ["/", atom_to_list(Mod)]).
-define(url2(Mod, Param), ["/", atom_to_list(Mod), "/", Param]).
