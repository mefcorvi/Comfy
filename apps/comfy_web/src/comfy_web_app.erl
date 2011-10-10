-module(comfy_web_app).
-behaviour(application).
-include("comfy_web_log.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?Log("Starting mirgames web application..."),
    comfy_web_sup:start_link().

stop(_State) ->
    ok.
