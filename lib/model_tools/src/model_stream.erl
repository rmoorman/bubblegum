-module(model_stream).

-export([find_last_by_key/3
        ,find_last_by_key/5
        ,find_last/2
        ,find_last/4
        ,create/4
        ,create/5
        ,delete/2
        ]).
-define(def_limit, 30).


find_last_by_key(Key, Scheme, Table) -> 
    find_last_by_key(Key, 0, ?def_limit, Scheme, Table).

find_last_by_key(Key, Offset, Limit, Scheme, Table) ->
    Query = select(value_ts,
                   Table, {key, $=, "$1"}, desc, {Offset, Limit}),
    {ok, _, Ans} = ppg:equery(Query, [Key]),
    {ok, [{jsonee:decode(Value, Scheme), Ts} ||
            {Value, Ts} <- Ans]}.

find_last(Scheme, Table) -> find_last(0, ?def_limit, Scheme, Table).
find_last(Offset, Limit, Scheme, Table) ->
    Query = select([id, key, value, ts],
                   Table, [], desc, {Offset, Limit}),
    {ok, _, Ans} = ppg:equery(Query, []),
    {ok, [{Id, Key, jsonee:decode(Value, Scheme), Ts} ||
            {Id, Key, Value, Ts} <- Ans]}.

create(Key, Value, Scheme, Table) ->
   Query = [<<"INSERT INTO ">>, atom_to_list(Table)
            ,<<" (key, value) VALUES ($1, $2) RETURNING id">>],
    {ok, _, _, [{Id}]} 
        = ppg:equery(Query, [Key, jsonee:encode(Value, Scheme)]),
    {ok, Id}.

create(Key, Value, Scheme, Timestamp, Table) ->
    Query = [<<"INSERT INTO ">>, atom_to_list(Table)
            ,<<" (key, ts, value) VALUES ($1, $2, $3, $4) RETURNING id">>],
    {ok, _, _, [{Id}]} 
        = ppg:equery(Query, [Key, Timestamp, jsonee:encode(Value, Scheme)]),
    {ok, Id}.

delete(Id, Table) ->
    Query = [<<"DELETE FROM ">>, atom_to_list(Table)
            ,<<" WHERE id = $1">>],
    ppg:equery(Query, [Id]).


%% Internal
select(Fields, Table, Where, Order, OffsetLimit) ->
    FieldsSQL = case Fields of
        value -> <<"value">>;
        ts    -> <<"ts">>;
        value_ts -> <<"value, ts">>;
        List  -> string:join(lists:map(fun atom_to_list/1, List), ", ")
    end,

    TableSQL = if
        is_atom(Table) -> atom_to_list(Table);
        true           -> Table
    end,

    WhereSQL = case where(Where) of
        [] -> [];
        V  -> [<<"WHERE ">>, V]
    end,
    OrderSQL = case Order of
        asc  -> <<"ORDER BY ts ASC">>;
        desc -> <<"ORDER BY ts DESC">>;
        _    -> ""
    end,

    LimitSQL = case OffsetLimit of
        "" -> "";
        {from, From}  -> [<<"OFFSET ">>, integer_to_list(From)];
        {to, Count}   -> [<<"LIMIT ">>, integer_to_list(Count)];
        {From, Count} -> [<<"OFFSET ">>, integer_to_list(From)
                         ,<<" LIMIT ">>, integer_to_list(Count)]
    end,

    _Query = [<<"SELECT ">>, FieldsSQL, <<" FROM ">>, TableSQL, <<" ">>
             ,WhereSQL, <<" ">>, OrderSQL, <<" ">>, LimitSQL].


where([]) -> [];
where([Head]) -> where(Head);
where([Head|Tail]) -> [<<"((">>, where(Head), <<") AND (">>, where(Tail), <<"))">>];
where({Field, Op, Value}) ->
    SqlOp = case Op of
        $< -> <<" < ">>;
        $> -> <<" > ">>;
        $= -> <<" = ">>
    end,
    [atom_to_list(Field), SqlOp, Value];
where({Field, in, Low, High}) -> [atom_to_list(Field), <<" BETWEEN (">>
                                 ,Low, <<") AND (">>, High, <<")">>].

        

