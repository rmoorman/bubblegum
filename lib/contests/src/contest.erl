-module(contest).

-include("contests.hrl").

-export([create/1
        ,save/1
        ,load/1
        ,empty/0
        ,id/1
        ,name/1
        ,name/2
        ,body/1
        ,body/2
        ,problems/1
        ,problems/2
        ,dict/1
        ,dict/2
        ,code/1
        ,code/2
        ,from_json/1
        ,to_json/1
        ,find_from/2
        ,find_till/2
        ,merge/2
        ]).

create(R) ->
    Id = model_kv_pg:alloc(contests),
    R1 = R#contest{id = Id},
    model_kv_pg:update(Id, R1, ?contest, contests),
    R1.

save(R) ->
    model_kv_pg:update(R#contest.id, R, ?contest, contests).

load(Id) ->
    model_kv_pg:read(Id, ?contest, contests).

empty() -> #contest{}.

id(R) -> R#contest.id.

name(R) -> R#contest.name.
name(Name, R) -> R#contest{name = Name}.

body(R) -> R#contest.body.
body(Body, R) -> R#contest{body = Body}.

problems(R) -> R#contest.problems.
problems(Problems, R) -> R#contest{problems = Problems}.

dict(R) -> R#contest.dict.
dict(Dict, R) -> R#contest{dict = Dict}.

code(R) -> R#contest.code.
code(Code, R) -> R#contest{code = Code}.

find_from(Key, Limit) ->
    model_kv_pg:find_from(Key, ?contest, contests, Limit).

find_till(Key, Limit) ->
    model_kv_pg:find_till(Key, ?contest, contests, Limit).

to_json(Rs) when is_list(Rs) ->
    jsonee:encode(Rs, [?contest]);
to_json(R) when is_tuple(R)->
    jsonee:encode(R, ?contest).

from_json(Json) ->
    jsonee:decode(Json, ?contest).

merge(New, Old) ->
    List = record_info(fields, contest),
    jsonee:merge(fun merge/3, List, New, Old).

merge(_, undefined, Old) -> Old;
merge(_, New, _)         -> New.

