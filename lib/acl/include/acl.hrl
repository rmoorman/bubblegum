
-include("../model_tools/include/model.hrl").

-type role_id() :: non_neg_integer(). 
-type resource_id() :: non_neg_integer(). 
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

-define(acl_role, ?jsonee(acl_role, [{member_of, [integer]}])).


-record(acl_resource, {
        id :: resource_id(),
        updated_by,
        updated_at,
        actions :: [acl_action()]
        }).

-define(acl_lists, {[allow, deny], [{allow, [integer]}, {deny, [integer]}]}).
-define(acl_action, {dict, string, ?acl_lists}).
-define(acl_resource, ?jsonee(acl_resource, [{actions, ?acl_action}])).

