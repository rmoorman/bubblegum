
-record(session, {token % should be first (check ets:new in session_keeper)
                 ,user
                 ,acl
                 ,bag % arbitary value
                 }).

