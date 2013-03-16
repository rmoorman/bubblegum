-module(model_file).
-export([create/1
        ,read/2
        ,delete/1
        ]).

-define(path, "priv/storage/").

create(File) ->
    Key = model_kv_pg:create(File, string, file_storage),
    Filename = ?path ++ Key,
    file:copy(File, Filename),
    Key.

read(Key, File) ->
    Filename = ?path ++ Key,
    file:copy(Filename, File),
    ok.

delete(Key) ->
    model_kv_pg:delete(Key, file_storage),
    Filename = ?path ++ Key,
    file:delete(Filename).


