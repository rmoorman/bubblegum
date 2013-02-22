-module(m1361220150_create_role_resource).
-export([upgrade/1, downgrade/1]).
-behaviour(sql_migration).

upgrade(C) ->
    io:format("Hi!~n~n", []),
    List = 
       [
            "CREATE TABLE acl_role (
                role_key BIGSERIAL NOT NULL,
                role_val text
            )",
            "ALTER TABLE ONLY acl_role
                ADD CONSTRAINT acl_role_pkey PRIMARY KEY (role_key)",
            
            "CREATE TABLE acl_resource (
                resource_key BIGSERIAL NOT NULL,
                resource_val text
            )",
            "ALTER TABLE ONLY acl_resource
                ADD CONSTRAINT acl_resource_pkey PRIMARY KEY (resource_key)"
        ],
    [pgsql:squery(C, Q) || Q <- List].

downgrade(C) ->
    List = 
        [
            "DROP TABLE acl_role",
            "DROP TABLE acl_resource"
        ],
    [pgsql:squery(C, Q) || Q <- List].
            

    

