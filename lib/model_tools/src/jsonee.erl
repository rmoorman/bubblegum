%% @doc jsonee -- convert erlang terms to eep18 and json by scheme
%%
%% This module is useful for converting tuples and records to format
%% which could be converted to json by jiffy later on.
%%
%% You know that in erlang internal representation of record is tuple
%% 
%%      -record(key_value, {
%%          key,
%%          value}).
%%
%%      {key_value, one, "two"} = #key_value{key = one, value = "two"}
%% 
%% If you want convert this record to json with jiffy then you need to
%% represent it with use of eep18 notation:
%%
%%      {[{key, one}, {value, <<"two">>}]}
%%
%% So you need to implement two converters -- one from record to json
%% and one another in reverse direction. This is not really fun work, 
%% I thought.
%%
%% jsonee could make your live better (maybe).
%%
%% At first you need to create format for your record:
%% 
%%      -define(s, ?jsonee(key_value, [{key, atom}, {value, string}])).
%%
%% Now you can use encode and decode
%%
%%      Json = jsonee:encode(Record, ?s)
%%      NewRecord = jsonee:decode(Json, ?s)
%%
-module(jsonee).

-export([to_eep18/2
        ,from_eep18/2
        ,transform_eep18/2
        ,encode/1
        ,encode/2
        ,decode/1
        ,decode/2
        ,merge/4
        ,to_mochi/2
        ,from_mochi/2
        ]).

-include("model.hrl").

%% Some local-scope defines
-define(a2b(Atom), list_to_binary(atom_to_list(Atom))).

-type format() :: id | undefined | atom | integer | string | uuid
                | [format()]  % list 
                | {[atom()], [{atom(), format()}]} % tuple
                | {dict, atom(), format()}
                | {record, atom(), [atom()], [{atom(), format()}]}. % record

%% Encode
-spec encode(term(), format()) -> binary().
encode(Var, Format) ->
    jiffy:encode(to_eep18(Var, Format)).

-spec encode(term()) -> {ok, binary()}.
encode(Var) ->
    {ok, jiffy:encode(Var)}.

% Simple structs
-spec to_eep18(term(), format()) -> term().
to_eep18(Var, id)        -> Var;
to_eep18(Var, undefined) -> Var;
to_eep18(Var, uuid)      -> to_eep18(Var, string);
to_eep18(Var, atom)    when is_atom(Var)    -> Var;
to_eep18(Var, integer) when is_integer(Var) -> Var;
to_eep18(Var, integer) when is_list(Var)    -> list_to_integer(Var);
to_eep18(Var, string)  when is_list(Var)    -> list_to_binary(Var);
to_eep18(Var, string)  when is_binary(Var)  -> Var;
to_eep18(Var, string)  when is_atom(Var)    -> list_to_binary(atom_to_list(Var));
to_eep18(Var, string)  when is_integer(Var) -> list_to_binary(integer_to_list(Var));

% Complex structs
to_eep18(Var, [])       -> Var;
to_eep18(Var, [Format]) -> [to_eep18(Item, Format) || Item <- Var];

to_eep18(Var, {dict, FmtK, FmtV}) 
        when (FmtK == atom) or (FmtK == string) or (FmtK == integer) ->
    {[{to_eep18(K, FmtK), to_eep18(V, FmtV)}
      || {K, V} <- Var]};

to_eep18(Var, {record, Record, Fields}) -> to_eep18(Var, {record, Record, Fields, []});
to_eep18(Var, {record, _Record, Fields, FormatsDict}) -> 
    FD = [{'?tuple', skip} | FormatsDict],     % Record is a tuple, skip the record
    to_eep18(Var, {['?tuple' | Fields], FD});  % signature

to_eep18(Var, {Fields})  -> to_eep18(Var, {Fields, []});
to_eep18(Var, {Fields, FormatsDict}) ->
    Vals = tuple_to_list(Var),
    {[{Name, to_eep18(Val, Fmt)} ||
        {Name, Val} <- lists:zip(Fields, Vals),
        Val /= undefined,
        begin
            Fmt = proplists:get_value(Name, FormatsDict, id),
            skip /= Fmt
        end]}.


%% Decode
-spec decode(binary(), format()) -> term().
decode(Bin, Format) ->
    from_eep18(jiffy:decode(Bin), Format).

-spec decode(binary()) -> {ok, term()} | {error, bad_json}.
decode(Bin) ->
    try {ok, jiffy:decode(Bin)}
    catch
        _:_ ->
            {error, bad_json}
    end.

-spec from_eep18(term(), format()) -> term().
from_eep18(V, id)        -> V;
from_eep18(V, undefined) -> V;
from_eep18(V, atom)    when is_binary(V)  -> list_to_atom(binary_to_list(V));
from_eep18(V, integer) when is_integer(V) -> V;
from_eep18(V, string)  when is_binary(V)  -> binary_to_list(V);
from_eep18(V, string)  when is_integer(V) -> integer_to_list(V);
from_eep18(V, uuid)    when is_binary(V)  -> V;
from_eep18(undefined, uuid) -> undefined;
from_eep18(undefined, string) -> undefined;

from_eep18(V, []) -> V;
from_eep18(_V, {set, F}) -> F;
from_eep18(undefined, [_]) -> undefined;
from_eep18(V, [Format]) -> [from_eep18(I, Format) || I <- V];

from_eep18({V}, {dict, FmtK, FmtV}) 
        when (FmtK == atom) or (FmtK == string) or (FmtK == integer) ->
    [{from_eep18(Key, FmtK), from_eep18(Val, FmtV)}
     || {Key, Val} <- V];

from_eep18(V, {record, Record, Fields}) -> from_eep18(V, {record, Record, Fields, []});
from_eep18(V, {record, Record, Fields, FormatsDict}) ->
    FD = [{'?tuple', {set, Record}} | FormatsDict],  % Append record tag to tuple
    from_eep18(V, {['?tuple' | Fields], FD});

from_eep18(V, {Fields}) -> from_eep18(V, {Fields, []});
from_eep18({V}, {Fields, FormatsDict}) ->
    List = [from_eep18(proplists:get_value(?a2b(I), V, undefined), Fmt) || 
            I <- Fields,
            begin
                Fmt = proplists:get_value(I, FormatsDict, id),
                true
            end],
    list_to_tuple(List);
from_eep18(undefined, _) -> undefined.


%% transform_eep18
transform_eep18(What, Format) ->
    from_eep18(to_eep18(What, Format), Format).


%% Mochi json encoding format
from_mochi(What, Format) ->
    from_eep18(from_mochi(What), Format).

from_mochi(List) when is_list(List) ->
    [from_mochi(Item) || Item <- List];
from_mochi({struct, List}) ->
    {[{Key, from_mochi(Value)} || {Key, Value} <- List]};
from_mochi(Item) ->
    Item.

to_mochi(What, Format) ->
    to_mochi(to_eep18(What, Format)).

to_mochi(List) when is_list(List) ->
    [to_mochi(Item) || Item <- List];
to_mochi({List}) ->
    {struct, [{Key, to_mochi(Value)} || {Key, Value} <- List]};
to_mochi(Item) ->
    Item.



%% Merge
%% Merge by fact has nothing to json,
%% but I don't know about better module where I can place It.
merge(Function, List, R1, R2) ->
    [Tag|L1] = tuple_to_list(R1),
    [Tag|L2] = tuple_to_list(R2),
    Merged = lists:zipwith3(Function, List, L1, L2),
    tuple_to_list([Tag|Merged]).
                      
%% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-record(r1, {id, a, b}).
%-define(r1, {record, r1, record_info(fields, r1), [{b, string}]}).
-define(r1, ?jsonee(r1, [{b, string}])).

-record(r2, {id, a, lstofr1}).
%-define(r2, {record, r2, record_info(fields, r2), [{lstofr1, [?r1]}]}).
-define(r2, ?jsonee(r2, [{lstofr1, [?r1]}])).

-define(dicted, {dict, string, [integer]}).

simple_test() ->
    T = #r1{id = 1, a = 2, b = "3"},
    Json = encode(T, ?r1),
    io:format("JSON: ~p~n", [Json]),
    TT = decode(Json, ?r1),
    io:format("REC:  ~p~n", [TT]),
    ?assert(TT == T),
    ok.

nested_test() ->
    T1 = #r1{id = 1, a = 2, b = "WaWa"},
    T2 = #r1{id = 2, a = 4, b = "HaHA"},
    S  = #r2{id = 1, a = 1, lstofr1 = [T1, T2]},
    Json = encode(S, ?r2),
    io:format("JSON: ~p~n", [Json]),
    SS = decode(Json, ?r2),
    io:format("REC:  ~p~n", [SS]),
    ?assert(SS == S),
    ok.

dict_test() ->
    D = [{"one", [1,10,11,12,14,15]}
        ,{"two", [2, 12]}
        ,{"three", [3, 13]}
        ,{"four", [4, 14]}
        ],
    Json = encode(D, ?dicted),
    io:format("JSON: ~p~n", [Json]),
    DD = decode(Json, ?dicted),
    io:format("REC:  ~p~n", [DD]),
    ?assert(DD == D),
    ok.


tuple_test() ->
    T = {[1,2,3], [4,5,6]},
    F = {[allow, deny], [{allow, [integer]}, {deny, [integer]}]},
    Json = encode(T, F),
    io:format("JSON: ~p~n", [Json]),
    TT = decode(Json, F),
    io:format("REC:  ~p~n", [TT]),
    ?assert(TT == T),
    ok.
    

-endif.

