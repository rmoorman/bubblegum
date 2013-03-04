
-include_lib("model_tools/include/model.hrl").

-record(profile, {id
                 ,login
                 ,email
                 ,role
                 ,resource
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
                                  ,{resource, uuid}
                                  ,{salt, string}
                                  ,{password, string}
                                  ,?profile_dict
                                  ])).
