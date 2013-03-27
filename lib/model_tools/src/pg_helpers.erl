% Some sql helpers
% Use it only in model_*_pg.erl
%
-module(pg_helpers).
-export([select/5
        ,where/1
        ]).

-include("model.hrl").

% Generic sql-query builder for select
select(Fields, Table, Where, Order, OffsetLimit) ->
    FieldsSQL = fields(Fields),
    TableSQL = if
        is_atom(Table) -> atom_to_list(Table);
        true           -> Table
    end,
    WhereSQL = case where(Where) of
        [] -> [];
        V  -> [<<" WHERE ">>, V]
    end,
    OrderSQL = case Order of
        {asc, F}  -> [<<" ORDER BY ">>, fields(F), <<" ASC">>];
        {desc, F} -> [<<" ORDER BY ">>, fields(F), <<" DESC">>];
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

% key < value < ts
% this order
fields(Fields) ->
    case Fields of
        key   -> <<"key">>;
        value -> <<"value">>;
        ts    -> <<"ts">>;
        value_ts  -> <<"value, ts">>;
        key_value -> <<"key, value">>;
        key_ts    -> <<"key, ts">>;
        key_value_ts -> <<"key, value, ts">>;
        List when is_list(List) -> 
            string:join(lists:map(fun atom_to_list/1, List), ", ")
    end.

where([]) -> [];
where([Head]) -> where(Head);
where([Head|Tail]) -> [<<"((">>, where(Head), <<") AND (">>, where(Tail), <<"))">>];
where({$!, Cond}) -> [<<" NOT (">>, where(Cond), <<")">>];
where({Field, Op, Value}) ->
    SqlOp = case Op of
        $<  -> <<" < ">>;
        $>  -> <<" > ">>;
        $=  -> <<" = ">>;
        lt  -> <<" <  ">>; % shell style
        lte -> <<" <= ">>;
        eq  -> <<" =  ">>;
        gte -> <<" >= ">>;
        gt  -> <<" >  ">>;
        neq -> <<" != ">>
    end,
    [atom_to_list(Field), SqlOp, Value];
where({Field, in, Low, High}) -> [atom_to_list(Field), <<" BETWEEN (">>
                                 ,Low, <<") AND (">>, High, <<")">>].

