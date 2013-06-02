-module(funtester_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ToStart = [sasl, crypto,
               confetti, pg_pool, model_tools, contests,
               acl, profiles, problems, session, problem_queue,
               ranch, cowboy],
    ToLoad  = [],

    [application:load(App)  || App <- ToLoad ++ ToStart],
    [ok = application:start(App) || App <- ToStart],

    cowboy_wrapper:start(),

    funtester_sup:start_link().

stop(_State) ->
    ok.
