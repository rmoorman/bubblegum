%
%   session
% 

-module(session_handler).

% Cowboy RESTs callbacks
-export([init/3
        ,allowed_methods/2
        ,content_types_provided/2
        ,content_types_accepted/2
        ,resource_exists/2
        ,post_is_create/2
        ,delete_resource/2
        ]).
% Callbacks for callbacks
-export([to_json/2
        ,from_json/2
        ]).

% Session API
-export([session/2
        ]).

-include_lib("model_tools/include/model.hrl").

-define(cookie, <<"session_handle">>).

%% Session API for other handlers
%%
%% Maybe I should create another library
%% which would be works with cookies...
session(R, S) ->
    {St, R1} = cowboy_req:cookie(?cookie, R),
    Now = erlang:now(),
    case St of
        undefined ->
            {ok, Role} = rolename:get_role(anonymous),
            {ok, Session} = session:create(<<>>, Role, % TODO user id
                                           [{created, Now}, {last, Now}]),
            R2 = cowboy_req:set_resp_cookie(?cookie, Session:token()
                                           ,[{http_only, true}], R1);
        Handle ->
            R2 = R1,
            {ok, Session} = session:load(Handle)
    end,
    {Session, R2, S}.

%% Cowboy API
init(_, _, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(R, S) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>
     ], R, S}.

content_types_provided(R, S) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}
     ], R, S}.

content_types_accepted(R, S) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}
     ], R, S}.

post_is_create(R, S) ->
    {true, R, S}.

resource_exists(R, _S) ->
    {St, Req} = cowboy_req:cookie(?cookie, R),
    State = case St of
        undefined -> Ans = false, undefined;
        Handle -> case session:load(Handle) of
                {ok, Session} -> Ans = true, Session;
                _ -> Ans = false, undefined
            end
    end,
    {Ans, Req, State}.

% GET /session
to_json(R, undefined = S) ->
    Ans = <<"{\"error\":\""
            "Please PUT your credintials to /session "
            "in appropriate json format"
            "\"}\n">>,
    {Ans, R, S};
to_json(R, Session) ->
    User = Session:user(),
    R2 = cowboy_req:set_resp_header(<<"location">>, <<"/user/", User/binary>>, R),
    {[], R2, Session}.

-record(post, {user, password}).
-define(post, ?jsonee(post, [{user, string}, {password, string}])).

% PUT /session
from_json(R, undefined) ->
    {ok, Body, R2} = cowboy_req:body(R),
    {post, User, Pass} = jsonee:decode(Body, ?post),
    {ok, Uuid} = profile:find_by_email(User),
    {ok, Profile} = profile:load(Uuid),
    true = Profile:check_password(Pass),
    Now = erlang:now(),
    Role = Profile:role(),
    {ok, Session} = session:create(Uuid, Role, [{created, Now}, {last, Now}]),
    R3 = cowboy_req:set_resp_header(<<"location">>, <<"/user/", Uuid/binary>>, R2),
    R4 = cowboy_req:set_resp_cookie(?cookie, Session:token()
                                   ,[{http_only, true}], R3),
    %{ok, R5} = cowboy_req:reply(301, R4),
    {true, R4, Session};

from_json(R, Session) ->
    {false, R, Session}.


% DELETE /session
delete_resource(R, Session) ->
    R1 = cowboy_req:set_resp_cookie(?cookie, "", [{http_only, true}
                                                 ,{expires, 0}], R),
    Session:close(),
    {true, R1, Session}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

