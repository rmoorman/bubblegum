-module(model_file).
-export([create/1
        ,read/2
        ,delete/1
        ]).

-define(path, "priv/storage/").

path(Key) when is_binary(Key) -> path(binary_to_list(Key));
path(Key) when is_atom(Key) -> path(atom_to_list(Key));
path(Key) when is_list(Key) -> ?path ++ Key.

create(File) ->
    Key = model_kv_pg:create(File, string, file_storage),
    Filename = path(Key),
    io:format("Filename: ~p~n", [Filename]),
    file:copy(File, Filename),
    binary_to_list(Key).

read(Key, File) ->
    Filename = path(Key),
    file:copy(Filename, File),
    ok.

delete(Key) ->
    model_kv_pg:delete(Key, file_storage),
    Filename = path(Key),
    file:delete(Filename).


