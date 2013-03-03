
-include("../model_tools/include/model.hrl").

-type role_id() :: binary(). 
-type resource_id() :: binary(). 
-type action_name() :: atom().
-type acl_action() :: {action_name(), {[role_id()], [role_id()]}}.
%%                                      ^            ^
%%                                      +-- allow    +-- deny

-record(acl_role, {
        id :: role_id(),
        updated_by,
        updated_at,
        member_of :: [role_id()]
        }).

-define(acl_role, ?jsonee(acl_role, [{id, uuid}
                                    ,{member_of, [uuid]}])).


-record(acl_resource, {
        id :: resource_id(),
        updated_by,
        updated_at,
        actions :: [acl_action()]
        }).

-define(acl_lists, {[allow, deny], [{allow, [uuid]}, {deny, [uuid]}]}).
-define(acl_action, {dict, string, ?acl_lists}).
-define(acl_resource, ?jsonee(acl_resource, [{id, uuid}
                                            ,{actions, ?acl_action}])).

