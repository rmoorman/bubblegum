
-define(OUT(Poolpool), poolpool:checkout(Poolpool)).
-define(IN(Poolpool, Worker), poolpool:checkin(Poolpool, Worker)).
