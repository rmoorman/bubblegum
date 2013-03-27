% Generic REST handler for cowboy
%
%   RSRS
% 
% Replace all RSRS to your_resource_name

-module(RSRSs_handler).

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

-import_lib("model_tools/include/model.hrl").

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
    case cowboy_req:binding(RSRS_id, R) of
        {undefined, R2} ->
            {true, R2, index};
        {Id, R2} ->
            case RSRS:load(Id) of
                {error, _} ->
                    {false, R2, {id, Id}};
                {ok, Resource} ->
                    {true, R2, {resource, Resource}}
            end
    end.

post_is_create(R, S) ->
    {true, R, S}.

create_path(R, _S) ->
    Uuid = RSRS:alloc(),
    {<<"/RSRS/", Uuid/binary>>, R, {id, Uuid}}.

% GET /RSRS/:RSRS_id
to_json(R, {resource, Resource} = S) ->
    {Resource:to_json(), R, S};

% GET /RSRSs
% GET /RSRSs?order=asc
% GET /RSRSs?order=desc
% GET /RSRSs?order=asc&from=ID
% GET /RSRSs?order=desc&till=ID
to_json(R, index = S) ->
    {From, R2} = cowboy_req:qs_val(<<"from">>, R, ?uuid_min),
    {Till, R3} = cowboy_req:qs_val(<<"till">>, R2, ?uuid_max),
    {Order, R4}= cowboy_req:qs_val(<<"order">>, R3, <<"asc">>),
    Ans = case Order of
        <<"asc">>  -> RSRS:find_from(From);
        <<"desc">> -> RSRS:find_till(To)
    end,
    {RSRS:to_json(Ans), R4, S}.

% PUT /RSRS/:RSRS_id
from_json(R, {resource, Resource}) ->
    {ok, Body, R2} = cowboy_req:body(R),
    NewResource = Resource:merge(RSRS:from_json(Body)),
    NewResource:save(),
    {true, R2, {resource, NewResource}};

% POST /RSRSs
from_json(R, {id, Id}) ->
    {ok, Body, R2} = cowboy_req:body(R),
    Resource1 = RSRS:from_json(Body),
    Resource2 = Resource1:id(Id),
    Resource2:save(),
    {true, R2, {resource, Resource2}}.

% DELETE /RSRS/:RSRS_id
delete_resource(R, {resource, Resource}) ->
    Deleted = Resource:deleted(true),
    Deleted:save(),
    {true, R, {resource, Deleted}}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
