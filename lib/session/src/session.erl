-module(session).

-export([user/1
        ,user/2
        ,token/1 % not token/2
        ,acl/1
        ,acl/2
        ,bag/1
        ,bag/2
        ,load/1
        ,save/1
        ,create/3
        ,close/1
        ]).

-include("session.hrl").

user(#session{user = User}) -> User.
user(User, R) -> R#session{user = User}.

token(#session{token = Token}) -> Token.
%token(Token, R) -> R#session{token = Token}.

acl(#session{acl = ACL}) -> ACL.
acl(ACL, R) -> R#session{acl = ACL}.

bag(#session{bag = Bag}) -> Bag.
bag(Bag, R) -> R#session{bag = Bag}.

load(Handler) ->
    session_keeper:load(Handler).

create(User, Acl, Bag) ->
    Session = #session{user = User, acl = Acl, bag = Bag},
    session_keeper:create(Session).

save(R) ->
    session_keeper:save(R).

close(R) ->
    session_keeper:close(R).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

one_test() ->
    {ok, Pid} = session_keeper:start_link(),
    {ok, R} = create("uuid", "another", []),
    {ok, R2} = load(R:token()),
    ?assertEqual(R2:user(), R:user()),
    R3 = R:bag([aaa]),
    ok = R3:save(),
    {ok, R4} = load(R:token()),
    ?assertEqual(R4:bag(), R3:bag()),
    session_keeper:stop(),
    ok.


-endif.
