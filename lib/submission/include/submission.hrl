-include_lib("model_tools/include/model.hrl").

-record(submission,
        {id
        ,problem
        ,ts
        ,file
        ,verdict}).

-define(submission,
        ?jsonee(submission, [{id, uuid}
                            ,{problem, uuid}
                            ,{ts, timestamp}
                            ,{file, uuid}
                            ,{verdict, [float]}])).
