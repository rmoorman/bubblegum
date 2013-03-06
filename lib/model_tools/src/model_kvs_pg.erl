-module(model_kvs_pg).

-export([add_value/3
        ,delete_value/3
        ,find_by_key/2
        ,find_by_value/2
        ,exists/3
        ,update/4]).

%TODO

add_value(Key, Value, Table) ->
    Query = lists:append(["INSERT INTO ", atom_to_list(Table),
                          " (key, value) VALUES ($1, $2)"]),
    _A = ppg:equery(Query, [Key, Value]).

delete_value(Key, Value, Table) ->
    Query = lists:append(["DELETE FROM ", atom_to_list(Table),
                          " WHERE key = $1 AND value = $2"]),
    _A = ppg:equery(Query, [Key, Value]).

find_by_key(Key, Table) ->
    Query = lists:append(["SELECT value FROM ", atom_to_list(Table),
                          " WHERE key = $1"]),
    {ok, _, Out} = ppg:equery(Query, [Key]),
    [Value || {Value} <- Out].

find_by_value(Value, Table) ->
    Query = lists:append(["SELECT key FROM ", atom_to_list(Table),
                          " WHERE value = $1"]),
    {ok, _, Out} = ppg:equery(Query, [Value]),
    [Key || {Key} <- Out].

exists(Key, Value, Table) ->
    Query = lists:append(["SELECT COUNT(1) ", atom_to_list(Table),
                          " WHERE key = $1 and value = $2"]),
    {ok, _, [{Count}]} = ppg:equery(Query, [Key, Value]),
    Count.

update(Key, Old, New, Table) when is_list(Old) and is_list(New) ->
    Add = ordsets:subtract(New, Old),
    Del = ordsets:subtract(Old, New),
    Insert = lists:append(["INSERT INTO ", atom_to_list(Table),
                           " (key, value) VALUES ($1, $2)"]),
    Delete = lists:append(["DELETE FROM ", atom_to_list(Table),
                           " WHERE key = $1 AND value = $2"]),
    perform(Key, Add, Insert),
    perform(Key, Del, Delete).

perform(Key, Vals, Query) ->
    {ok, Statement} = pgsql:parse(ppg:get_conn(), Query),
    [begin C = ppg:get_conn(),
           pgsql:bind(C, Statement, [Key, Val]),
           pgsql:execute(C, Statement)
     end || Val <- Vals].
