-module(funtester_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ToStart = [sasl, confetti, pg_pool, model_tools,
               acl, nprocreg, profiles, problems],
    ToLoad  = [mochiweb],

    [application:load(App)  || App <- ToLoad ++ ToStart],
    [application:start(App) || App <- ToStart],

    funtester_sup:start_link().

stop(_State) ->
    ok.
