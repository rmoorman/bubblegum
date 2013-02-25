%% @doc Mochiweb server starter
%%
%% Reads config and starts some mochi

-module(mochiweb_wrapper).

-export([start_link/0
        ,stop/1
        ,process/1
        ]).

-define(doc_root, "priv/webroot").

% For supervisor API
start_link() ->
    Env = os:getenv("MOCHIWEB_CONFIG"),
    ConfigName = if
        Env /= false -> Env;
        true -> "mochiweb.conf"
    end,
    {ok, _} = confetti:use(mochiweb_conf, [
                {location, {ConfigName, "conf"}},
                {subscribe, false}]),
    Conf = confetti:fetch(mochiweb_conf),

    Options = Conf
              ++ [{loop, fun ?MODULE:process/1}],

    % Start the server
    mochiweb_http:start_link(Options).

stop(Pid) ->
    mochiweb_http:stop(Pid).

% Entry point to request processing
process(Req) ->
    ReqBridge = simple_bridge:make_request(mochiweb_request_bridge, {Req, ?doc_root}),
    ResBridge = simple_bridge:make_response(mochiweb_response_bridge, {Req, ?doc_root}),
    nitrogen:init_request(ReqBridge, ResBridge),

    nitrogen:run().

