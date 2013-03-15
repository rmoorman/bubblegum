-module(model_stream).

-export([find_last_by_key/2
        ,find_last_by_key/4
        ,find_last/1
        ,find_last/3
        ,create/4
        ,delete/3
        ]).
-define(def_limit, 30).


find_last_by_key(Key, Table) -> find_last_by_key(Key, 0, ?def_limit, Table).
find_last_by_key(Key, Offset, Limit, Table) ->
    Query = select(value_ts, Table, {key, $=, "$1"}, desc, {Offset, Limit}),
    {ok, _, Ans} = ppg:equery(Query, [Key]),
    {ok, Ans}.

find_last(Table) -> find_last(0, ?def_limit, Table).
find_last(Offset, Limit, Table) ->
    Query = select([key, value, ts], Table, [], desc, [Offset, Limit]),
    {ok, _, Ans} = ppg:equery(Query, []),
    {ok, Ans}.

create(Key, Value, Timestamp, Table) ->
    Query = [<<"INSERT INTO ">>, atom_to_list(Table)
            ,<<" (key, value, ts) VALUES ($1, $2, $3) ">>],
    ppg:equery(Query, [Key, Value, Timestamp]).

delete(Key, Value, Table) ->
    Query = [<<"DELETE FROM ">>, atom_to_list(Table)
            ,<<" WHERE key = $1 AND value = $2">>],
    ppg:equery(Query, [Key, Value]).

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

        

