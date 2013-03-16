
-define(jsonee(Name), 
        {record, Name, record_info(fields, Name)}).
-define(jsonee(Name, Types), 
        {record, Name, record_info(fields, Name), Types}).

-define(uuid2s(UUID), ["'", UUID, "'"]).
