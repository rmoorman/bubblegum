
%-include_lib("model_tools/include/model.hrl").
-include("../model_tools/include/model.hrl").

-record(contest, {id
                 ,name = ""
                 ,body = ""
                 ,problems = []
                 ,dict = []
                 ,code = []
                 }).

-define(contest, ?jsonee(contest, [{id, uuid}
                                  ,{name, string}
                                  ,{body, string}
                                  ,{problems, {dict, string, uuid}}
                                  ,{dict, {dict, string, id}}
                                  ,{code, string}
                                  ])).
