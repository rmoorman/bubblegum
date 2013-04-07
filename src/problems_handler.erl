%
%   problem
% 

-module(problems_handler).

% Cowboy RESTs callbacks
-export([init/3
        ,allowed_methods/2
        ,content_types_provided/2
        ,content_types_accepted/2
        ,resource_exists/2
        ,post_is_create/2
        ,create_path/2
        ,delete_resource/2
        ]).
% Callbacks for callbacks
-export([to_json/2
        ,from_json/2
        ]).

-include_lib("model_tools/include/model.hrl").

init(_, _, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(R, S) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>
     ], R, S}.

content_types_provided(R, S) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}
     ], R, S}.

content_types_accepted(R, S) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}
     ], R, S}.

resource_exists(R, S) ->
    case cowboy_req:binding(problem_id, R) of
        {undefined, R2} ->
            {true, R2, index};
        {Id, R2} ->
            case problem:load(Id) of
                {error, _} ->
                    {false, R2, {id, Id}};
                {ok, Resource} ->
                    {true, R2, {resource, Resource}}
            end
    end.

post_is_create(R, S) ->
    {true, R, S}.

create_path(R, _S) ->
    Uuid = problem:alloc(),
    {<<"/problem/", Uuid/binary>>, R, {id, Uuid}}.

% GET /problem/:problem_id
to_json(R, {resource, Resource} = S) ->
    {Resource:to_json(), R, S};

% GET /problems
% GET /problems?order=asc
% GET /problems?order=desc
% GET /problems?order=asc&from=ID
% GET /problems?order=desc&till=ID
to_json(R, index = S) ->
    {From, R2} = cowboy_req:qs_val(<<"from">>, R, ?uuid_min),
    {Till, R3} = cowboy_req:qs_val(<<"till">>, R2, ?uuid_max),
    {Order, R4}= cowboy_req:qs_val(<<"order">>, R3, <<"asc">>),
    {ok, Ans} = case Order of
        <<"asc">>  -> problem:find_from(From, ?def_limit);
        <<"desc">> -> problem:find_till(Till, ?def_limit)
    end,
    {problem:to_json(Ans), R4, S}.

% PUT /problem/:problem_id
from_json(R, {resource, Resource}) ->
    {ok, Body, R2} = cowboy_req:body(R),
    NewResource = Resource:merge(problem:from_json(Body)),
    NewResource:save(),
    {true, R2, {resource, NewResource}};

% POST /problems
from_json(R, {id, Id}) ->
    {ok, Body, R2} = cowboy_req:body(R),
    Resource1 = problem:from_json(Body),
    Resource2 = Resource1:id(Id),
    Resource2:save(),
    {true, R2, {resource, Resource2}}.

% DELETE /problem/:problem_id
delete_resource(R, {resource, Resource}) ->
    Deleted = Resource:deleted(true),
    Deleted:save(),
    {true, R, {resource, Deleted}};

% DELETE /problems
delete_resource(R, index = S) ->
    {false, R, S}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
