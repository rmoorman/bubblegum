
-type role_id() :: non_neg_integer() | binary(). 
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
        actions :: orddict:orddict()
        }).


