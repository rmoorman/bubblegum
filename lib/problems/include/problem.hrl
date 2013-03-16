
-include_lib("model_tools/include/model.hrl").

-record(problem, 
        {id
        ,name
        ,tags
        ,body
        ,file
        ,acl}).

-define(problem,
        ?jsonee(problem, [{id, uuid}
                         ,{name, string}
                         ,{tags, [string]}
                         ,{body, string}
                         ,{acl, uuid}
                         ,{file, uuid}])).

-define(problem_short,
        ?jsonee(problem, [{id, uuid}
                         ,{name, string}
                         ,{tags, [string]}
                         ,{body, skip}
                         ,{acl, uuid}
                         ,{file, uuid}])).


-record(input_line,
        {id
        ,type
        ,label
        ,comment
        ,variants
        }).

-define(input_line,
        ?jsonee(input_line, [{id, string}
                            ,{type, string}
                            ,{label, string}
                            ,{comment, string}
                            ,{variants, [string]}])).
-define(input, [?input_line]).

-record(output_line,
        {id
        ,type
        ,label
        ,comment
        }).
-define(output_line,
        ?jsonee(output_line, [{id, string}
                             ,{type, string}
                             ,{label, string}
                             ,{comment, string}])).


