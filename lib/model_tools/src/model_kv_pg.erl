-module(model_kv_pg).

-export([alloc/1
        ,alloc/2
        ,create/3
        ,create/4
        ,read/3
        ,update/4
        ,delete/2
        ]).

alloc(Table) ->
    Query = lists:flatten(["INSERT INTO ", atom_to_list(Table),
                           " (value) VALUES (NULL) RETURNING key"]),
    {ok, _, _, [{Key}]} = ppg:equery(Query),
    Key.

alloc(Key, Table) -> 
    Query = lists:flatten(["INSERT INTO ", atom_to_list(Table),
                           " (key, value) VALUES ($1, NULL)"]),
    ppg:equery(Query, [Key]),
    Key.

create(Value, Format, Table) ->
    Value = jsonee:encode(Value, Format),
    Query = lists:flatten(["INSERT INTO ", atom_to_list(Table),
                           " (value) VALUES ($1)"]),
    {ok, _, _, [{Key}]} = ppg:equery(Query, [Value]),
    Key.

create(Key, Value, Format, Table) ->
    Json = jsonee:encode(Value, Format),
    Query = lists:flatten(["INSERT INTO ", atom_to_list(Table),
                           " (key, value) VALUES ($1, $2)"]),
    {ok, _, _, [{Key}]} = ppg:equery(Query, [Key, Json]),
    Key.

read(Key, Format, Table) ->
    Query = lists:flatten(["SELECT value FROM ", atom_to_list(Table),
                           " WHERE key = $1"]),
    {ok, _, [{Value}|_]} = ppg:equery(Query, [Key]),
    jsonee:decode(Value, Format).

update(Key, NewValue, Format, Table) ->
    Json = jsonee:encode(NewValue, Format),
    Query = lists:flatten(["UPDATE ", atom_to_list(Table),
                           " SET value = $2 WHERE key = $1"]),
    {ok, _} = ppg:equery(Query, [Key, Json]),
    NewValue.

delete(Key, Table) ->
    Query = lists:flatten(["DELETE FROM ", atom_to_list(Table),
                           " WHERE key = $1"]),
    ppg:equery(Query, [Key]),
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
