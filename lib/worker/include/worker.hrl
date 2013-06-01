
-include_lib("submission/include/submission.hrl").

% Spec for system_make.json
%
-define(system_make, {dict, string, {dict, string, [string]}}).


% Spec for problem.json
-record(problem, {name, result}).
-define(problem, ?jsonee(problem, [{name, string}
                                  ,{result, ?verdict}
                                  ])).

