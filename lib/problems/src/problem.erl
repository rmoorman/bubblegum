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
        ]).

-include("problem.hrl").

empty() -> #problem{}.

id(R) -> R#problem.id.

name(R) -> R#problem.name.
name(Name, R) -> R#problem{name = Name}.

tags(R) -> R#problem.tags.
tags(Tags, R) -> R#problem{tags = Tags}.

body(R) -> R#problem.body.
body(Body, R) -> R#problem{body = Body}.

load(Id) -> model_kv_pg:read(Id, ?problem, problems).

save(R) -> save(load(R#problem.id), R). 
save(Old, R) ->
    model_kv_pg:update(R#problem.id, R, ?problem, problems).

create(R) ->
    Id = model_kv_pg:create(R, ?problem, problems),
    R#problem{id = Id}.

file(R) ->
    R#problem.file.
file(File, R) ->
    R#problem{file = File}.
