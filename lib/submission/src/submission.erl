-module(submission).

-include("submission.hrl").

-export([create/1
        ,save/1
        ,load/1
        ,empty/0
        ,problem/1
        ,problem/2
        ,ts/1
        ,ts/2
        ,file/1
        ,file/2
        ,verdict/1
        ,verdict/2]).

create(R) ->
    Id = model_kv_pg:alloc(submission),
    R1 = R#submission{id = Id},
    model_kv_pg:update(Id, R1, ?submission, submission),
    R1.

save(R) ->
    model_kv_pg:update(R#submission.id, R, ?submission, submission).

load(Id) ->
    model_kv_pg:read(Id, ?submission, submission).

empty() -> #submission{verdict = []}.

problem(R) -> R#submission.problem.
problem(Id, R) -> R#submission{problem = Id}.

ts(R) -> R#submission.ts.
ts(Id, R) -> R#submission{ts = Id}.

file(R) -> R#submission.file.
file(Id, R) -> R#submission{file = Id}.

verdict(R) -> R#submission.verdict.
verdict(Id, R) -> R#submission{verdict = Id}.

