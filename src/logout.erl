-module(logout).
-export([main/0]).
-include("web.hrl").

main() -> wf:logout(), wf:redirect("/").
