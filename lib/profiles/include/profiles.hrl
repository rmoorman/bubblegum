
-include_lib("model_tools/include/model.hrl").

-record(profile, {id
                 ,login
                 ,email
                 ,role
                 ,salt
                 ,password
                 ,dict
                 ,deleted
                 }).

-define(profile_dict, {dict, atom, string}).
-define(profile, ?jsonee(profile, [{id, uuid}
                                  ,{login, string}
                                  ,{email, string}
                                  ,{role, uuid}
                                  ,{salt, string}
                                  ,{password, string}
                                  ,?profile_dict
                                  ])).
