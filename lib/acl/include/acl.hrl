
-type role_id() :: non_neg_integer(). 
-type resource_id() :: non_neg_integer(). 
-type action_name() :: atom().
-type acl_action() :: {action_name(), {[role_id()], [role_id()]}}.

-record(acl_role, {
        id :: role_id(),
        updated_by,
        updated_at,
        member_of :: [role_id()]
        }).


-record(acl_resource, {
        id :: resource_id(),
        updated_by,
        updated_at,
        actions :: [acl_action()]
        }).


