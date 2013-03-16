
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

         % Some static
        ,{"/bootstrap", static_file}
        ,{"/nitrogen",  static_file}
        ,{"/js",        static_file}
        ,{"/css",       static_file}
        ,{"/images",    static_file}
        ]).

