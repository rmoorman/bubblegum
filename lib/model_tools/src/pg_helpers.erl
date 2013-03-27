% Some sql helpers
% Use it only in model_*_pg.erl
%
-module(pg_helpers.erl).
-export([select/5
        ,where/1
        ]).

% Generic sql-query builder for select
select(Fields, Table, Where, Order, OffsetLimit) ->
    % key < value < ts
    % this order
    FieldsSQL = case Fields of
        key   -> <<"key">>;
        value -> <<"value">>;
        ts    -> <<"ts">>;
        value_ts  -> <<"value, ts">>;
        key_value -> <<"key, value">>;
        key_ts    -> <<"key, ts">>;
        key_value_ts -> <<"key, value, ts">>;
        List when is_list(List) -> 
            string:join(lists:map(fun atom_to_list/1, List), ", ")
    end,

    TableSQL = if
        is_atom(Table) -> atom_to_list(Table);
        true           -> Table
    end,

    WhereSQL = case where(Where) of
        [] -> [];
        V  -> [<<" WHERE ">>, V]
    end,
    OrderSQL = case Order of
        asc  -> <<" ORDER BY ts ASC">>;
        desc -> <<" ORDER BY ts DESC">>;
        _    -> ""
    end,

    LimitSQL = case OffsetLimit of
        "" -> "";
        {from, From}  -> [<<" OFFSET ">>, integer_to_list(From)];
        {to,   Count} -> [<<" LIMIT ">>,  integer_to_list(Count)];
        {From, Count} -> [<<" OFFSET ">>, integer_to_list(From)
                         ,<<"  LIMIT ">>, integer_to_list(Count)]
    end,

    _Query = [<<"SELECT ">>, FieldsSQL, <<" FROM ">>, TableSQL
             ,WhereSQL, OrderSQL, LimitSQL].


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

