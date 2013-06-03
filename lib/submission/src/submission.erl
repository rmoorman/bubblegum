-module(submission).

-include("submission.hrl").

-export([create/1
        ,save/1
        ,load/1
        ,empty/0
        ,id/1
        ,problem/1
        ,problem/2
        ,ts/1
        ,ts/2
        ,file/1
        ,file/2
        ,verdict/1
        ,verdict/2
        ,user/1
        ,user/2
        ]).

create(R) ->
    Id = model_kv_pg:alloc(submissions),
    R1 = R#submission{id = Id},
    model_kv_pg:update(Id, R1, ?submission, submissions),
    R1.

save(R) ->
    model_kv_pg:update(R#submission.id, R, ?submission, submissions).

load(Id) ->
    model_kv_pg:read(Id, ?submission, submissions).

empty() -> #submission{verdict = []}.

id(R) -> R#submission.id.

problem(R) -> R#submission.problem.
problem(Id, R) -> R#submission{problem = Id}.

ts(R) -> R#submission.ts.
ts(Id, R) -> R#submission{ts = Id}.

file(R) -> R#submission.file.
file(Id, R) -> R#submission{file = Id}.

verdict(R) -> R#submission.verdict.
verdict(Id, R) -> R#submission{verdict = Id}.

user(R) -> R#submission.user.
user(U, R) -> R#submission{user = U}.
