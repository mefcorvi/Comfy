-module(comfy_server_app).
-behaviour(application).
-include("comfy_server.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?Log("Starting comfy server..."),
    couchbeam:start(),
    comfy_server_sup:start_link().

stop(_State) ->
    ok.

