
-define(jsonee(Name), 
        {record, Name, record_info(fields, Name)}).
-define(jsonee(Name, Types), 
        {record, Name, record_info(fields, Name), Types}).

% UUID stuff
-define(uuid2s(UUID), ["'", UUID, "'"]).
-define(uuid_min, "00000000-0000-0000-0000-000000000000").
-define(uuid_max, "ffffffff-ffff-ffff-ffff-ffffffffffff").

% Default SELECT's limit
-define(def_limit, 30).

