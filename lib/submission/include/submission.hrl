-include_lib("model_tools/include/model.hrl").

-record(submission,
        {id
        ,user
        ,problem
        ,contest
        ,ts
        ,file
        ,verdict}).

-define(verdict, {dict, string, id}).
-define(submission,
        ?jsonee(submission, [{id, uuid}
                            ,{problem, uuid}
                            ,{user, uuid}
                            ,{contest, uuid}
                            ,{ts, timestamp}
                            ,{file, uuid}
                            ,{verdict, ?verdict}])).
