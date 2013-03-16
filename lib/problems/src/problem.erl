-module(problem).
-export([empty/0
        ,id/1
        ,name/1
        ,name/2
        ,tags/1
        ,tags/2
        ,body/1
        ,body/2
        ,load/1
        ,save/1
        ,save/2
        ,create/1
        ,file/1
        ,file/2
        ,input/1
        ,input/2
        ,output/1
        ,output/2
        ]).

-include("problem.hrl").

empty() -> #problem{tags = [], input = [], output = []}.

id(R) -> R#problem.id.

name(R) -> R#problem.name.
name(Name, R) -> R#problem{name = Name}.

tags(R) -> R#problem.tags.
tags(Tags, R) ->
    case R#problem{tags = Tags} of
        undefined -> [];
        List -> List
    end.

body(R) -> R#problem.body.
body(Body, R) -> R#problem{body = Body}.

load(Id) -> 
    R = model_kv_pg:read(Id, ?problem, problems),
    R#problem{id = Id}.

save(R) -> save(load(R#problem.id), R). 
save(Old, R) ->
    model_kv_pg:update(R#problem.id, R, ?problem, problems).

create(R) ->
    Id = model_kv_pg:alloc(problems),
    model_kv_pg:update(Id, R#problem{id = Id}, ?problem, problems),
    R#problem{id = Id}.

file(R) ->
    R#problem.file.
file(File, R) ->
    Tmp = string:strip(os:cmd("mktemp"), right, 10),
    Dir = string:strip(os:cmd("mktemp -d"), right, 10),
    model_file:read(R#problem.file, Tmp),
    zip:extract(Tmp, [{cwd, Dir}]),
    file:delete(Tmp),
    R1 = case file:read_file(Dir ++ "/scheme_input.json") of
        {ok, JsonI} -> 
            R#problem{input = jsonee:decode(JsonI, ?input)};
        _ -> R
    end,
    R2 = case file:read_file(Dir ++ "/scheme_output.json") of
        {ok, JsonO} -> R1#problem{output = jsonee:decode(JsonO, ?output)};
        _ -> R1
    end,
    os:cmd("rm -Rf " ++ Dir), 
    R2#problem{file = File}.

input(R) ->
    R#problem.input.
input(Input, R) ->
    R#problem{input = Input}.

output(R) ->
    R#problem.output.
output(Output, R) ->
    R#problem{output = Output}.



