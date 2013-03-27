-module(model_kv_pg).

-export([alloc/1
        ,alloc/2
        ,create/3
        ,create/4
        ,read/3
        ,update/4
        ,delete/2
        ,exists/2
        ,find_from/3
        ,find_from/4
        ,find_till/3
        ,find_till/4
        ]).
-include("model.hrl").

alloc(Table) ->
    Query = ["INSERT INTO ", atom_to_list(Table),
             " (value) VALUES (NULL) RETURNING key"],
    {ok, _, _, [{Key}]} = ppg:equery(Query, []),
    Key.

alloc(Key, Table) -> 
    Query = ["INSERT INTO ", atom_to_list(Table),
             " (key, value) VALUES ($1, NULL)"],
    ppg:equery(Query, [Key]),
    Key.

create(Value, Format, Table) ->
    Json = jsonee:encode(Value, Format),
    Query = ["INSERT INTO ", atom_to_list(Table),
             " (value) VALUES ($1) RETURNING key"],
    {ok, _, _, [{Key}]} = ppg:equery(Query, [Json]),
    Key.

create(Key, Value, Format, Table) ->
    Json = jsonee:encode(Value, Format),
    Query = ["INSERT INTO ", atom_to_list(Table),
             " (key, value) VALUES ($1, $2)"],
    {ok, _} = ppg:equery(Query, [Key, Json]),
    Key.

read(Key, Format, Table) ->
    Query = ["SELECT value FROM ", atom_to_list(Table),
             " WHERE key = $1"],
    case ppg:equery(Query, [Key]) of
        {ok, _, [{null}]} ->
            {ok, null};
        {ok, _, [{Value}|_]} ->
            {ok, jsonee:decode(Value, Format)};
        {ok, _, []} ->
            {error, not_found};
        {error, Error} ->
            {error, Error}
    end.

update(Key, NewValue, Format, Table) ->
    Json = jsonee:encode(NewValue, Format),
    Query = ["UPDATE ", atom_to_list(Table),
             " SET value = $2 WHERE key = $1"],
    {ok, _} = ppg:equery(Query, [Key, Json]),
    NewValue.

delete(Key, Table) ->
    Query = ["DELETE FROM ", atom_to_list(Table),
             " WHERE key = $1"],
    ppg:equery(Query, [Key]),
    ok.

exists(Key, Table) ->
    Query = ["SELECT COUNT(1) FROM ", atom_to_list(Table),
             " WHERE key = $1"],
    {ok, _, [{Count}]} = ppg:equery(Query, [Key]),
    Count /= 0.

find_from(Key, Format, Table) ->
    find_from(Key, Format, Table, ?def_limit).

find_from(Key, Format, Table, Limit) ->
    Query = pg_helpers:select(key_value, Table, {key, gte, "$1"}
                             ,{asc, key}, {limit, Limit}),
    {ok, _, Answer} = ppg:equery(Query, [Key]),
    {ok, [jsonee:decode(Value, Format)
          || Value <- Answer]}.

find_till(Key, Format, Table) ->
    find_till(Key, Format, Table, ?def_limit).

find_till(Key, Format, Table, Limit) ->
    Query = pg_helpers:select(key_value, Table, {key, lte, "$1"}
                             ,{desc, key}, {limit, Limit}),
    {ok, _, Answer} = ppg:equery(Query, [Key]),
    {ok, [jsonee:decode(Value, Format)
          || Value <- Answer]}.
                              
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
