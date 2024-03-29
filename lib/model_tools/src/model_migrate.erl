-module(model_migrate).

-export([migration/1
        ,create_kv_table_uuid/1
        ,create_kv_table_string/1
        ,drop_kv_table/1
        ,create_kvs_table_uuid/1
        ,create_kvs_table_text/1
        ,drop_kvs_table/1
        ,create_stream_table_uuid/1
        ,drop_stream_table/1
        ]).

%% Key Value table
create_kv_table_uuid(Name) ->
    Query = "CREATE TABLE " ++
            atom_to_list(Name) ++ " (" ++
            "key uuid PRIMARY KEY DEFAULT uuid_generate_v4(), " ++
            "value text);",
    migration(Query).

create_kv_table_string(Name) ->
    Query = "CREATE TABLE " ++
            atom_to_list(Name) ++ " (" ++
            "key text PRIMARY KEY, " ++
            "value text);",
    migration(Query).

drop_kv_table(Name) ->
    Query = "DROP TABLE IF EXISTS " ++ atom_to_list(Name) ++";",
    migration(Query).


%% Key Values table
create_kvs_table_uuid(Name) ->
    QueryT = "CREATE TABLE " ++ atom_to_list(Name) ++ " (" ++
             "value uuid, " ++
             "key uuid, " ++
             "UNIQUE (key, value))",
    QueryI = "CREATE INDEX " ++ atom_to_list(Name) ++ "_value_btree " ++
             "ON " ++ atom_to_list(Name) ++ " USING btree (value)",
    migration(QueryT),
    migration(QueryI).

create_kvs_table_text(Name) ->
    QueryT = "CREATE TABLE " ++ atom_to_list(Name) ++ " (" ++
             "value TEXT, " ++
             "key uuid, " ++
             "UNIQUE (key, value))",
    QueryI = "CREATE INDEX " ++ atom_to_list(Name) ++ "_value_btree " ++
             "ON " ++ atom_to_list(Name) ++ " USING btree (value)",
    migration(QueryT),
    migration(QueryI).

drop_kvs_table(Name) ->
    Query = "DROP TABLE IF EXISTS " ++ atom_to_list(Name) ++";",
    migration(Query).


%% Stream
%% Stream is (key, value, date) with uniq (key, value).

create_stream_table_uuid(Name) ->
    QueryT = "CREATE TABLE " ++ atom_to_list(Name) ++ " (" 
             "id uuid PRIMARY KEY DEFAULT uuid_generate_v4(), " 
             "key uuid, " 
             "value TEXT, "
             "ts timestamp DEFAULT now()) ",
    QueryI1 = "CREATE INDEX " ++ atom_to_list(Name) ++ "_key_ts_btree " ++
              "ON " ++ atom_to_list(Name) ++ " USING btree (key, ts)",
    QueryI2 = "CREATE INDEX " ++ atom_to_list(Name) ++ "_ts_btree " ++
              "ON " ++ atom_to_list(Name) ++ " USING btree (ts)",
    migration(QueryT),
    migration(QueryI1),
    migration(QueryI2).

drop_stream_table(Name) ->
    Query = "DROP TABLE IF EXISTS " ++ atom_to_list(Name) ++";",
    migration(Query).

migration(Query) ->
    error_logger:info_msg("Migration:~n~s~n", [Query]),
    case ppg:squery(Query) of
        {error, Error} ->
            error_logger:error_msg("While doing ~n    ~s~n"
                                   "An error occured has occured:~n    ~p~n",
                                   [Query, Error]),
            {error, Error};
        _Ok -> ok
    end.

