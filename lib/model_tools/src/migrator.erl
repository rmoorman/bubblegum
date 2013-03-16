%% SQL migrations
%%
%% Inspired by erlang-sql-migrations by yrashk
%% https://github.com/spawngrid/erlang-sql-migrations/
%%
-module(migrator).

-export([behaviour_info/1
        ,migrations/0
        ,migrations/1
        ,installed_migrations/0
        ,downgrade/1
        ,upgrade/1
        ,downgrade/0
        ,upgrade/0
        ,reset/0
        ]).


behaviour_info(callbacks) ->
    [{upgrade, 1}
    ,{downgrade, 1}];
behaviour_info(_) -> undefined.


%% @doc Get list of migrations of all loaded applications
migrations() ->
    Apps = [App || {App, _, _} <- application:loaded_applications()],
    migrations(Apps).

migrations(List) when is_list(List) ->
    lists:usort(lists:flatten([migrations(M) || M <- List]));
migrations(App) ->
    {ok, Modules} = application:get_key(App, modules),
    Migrations = [M || M <- Modules,
                     lists:member(migrator
                                 ,proplists:get_value(behaviour
                                                      ,M:module_info(attributes), []))],
    lists:usort(Migrations).

installed_migrations() ->
    Query = <<"SELECT migration_id FROM migrations ORDER BY migration_id DESC">>,
    case ppg:squery(Query) of
        {error, {error, error, <<"42P01">>, _, _}} ->
            install_table(),
            [];
        {ok, _, List} ->
            [list_to_atom(binary_to_list(Module)) || {Module, _} <- List]
    end.

install_table() ->
    Query1 = "CREATE TABLE migrations ("
             "migration_id VARCHAR PRIMARY KEY,"
             "datetime TIMESTAMP DEFAULT CURRENT_TIMESTAMP)",
    Query2 = "CREATE TABLE migrations_log ("
             "log_id SERIAL PRIMARY KEY, "
             "migration_id VARCHAR, "
             "action VARCHAR, "
             "event VARCHAR, "
             "comment VARCHAR, "
             "datetime TIMESTAMP DEFAULT CURRENT_TIMESTAMP)",
    ppg:squery(Query1),
    ppg:squery(Query2).

install_migration(ModuleName, Action)
        when Action == upgrade; Action == downgrade ->
    Query = "INSERT INTO migrations_log "
             "(migration_id, action, event, comment) VALUES ($1, $2, $3, $4) "
             "RETURNING log_id",

    ppg:equery(Query, [atom_to_list(A) || A <- [ModuleName, Action, start, '']]),
    Connection = ppg:get_conn(),
    case ModuleName:Action(Connection) of
        ok -> 
            ppg:equery(Query,
                       [atom_to_list(A) || A <- [ModuleName, Action, finish_ok, '']]),
            ok;
        {error, Error} ->
            ppg:equery(Query,
                       [atom_to_list(A) || A <- [ModuleName, Action, finish_error]]
                       ++ [lists:flatten(io_lib:print((Error)))]),
            {error, Error}
    end.

upgrade(Atom) when is_atom(Atom) -> upgrade([Atom]);
upgrade(List) ->
    AlreadyInstalled = installed_migrations(),
    ToInstall = List -- AlreadyInstalled,
    io:format("Installing: ~p~n", [ToInstall]),
    [install_migration(M, upgrade) || M <- ToInstall].

downgrade(Atom) when is_atom(Atom) -> downgrade([Atom]);
downgrade(List) ->
    Installed = installed_migrations(),
    ToUninstall = List -- (List -- Installed),
    io:format("Uninstalling: ~p~n", [ToUninstall]),
    [install_migration(M, downgrade) || M <- ToUninstall].

upgrade(  ) -> upgrade(migrations()).
downgrade() -> downgrade(migrations()).

reset() -> {downgrade(), upgrade()}.
