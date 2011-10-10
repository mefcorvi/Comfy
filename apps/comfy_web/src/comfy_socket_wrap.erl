-module(comfy_socket_wrap).
-export([send/2, send_term/2, wrap/1]).
-include("comfy_web_log.hrl").

wrap(Ws) ->
    {comfy_socket_wrap, Ws}.

send(Data, {comfy_socket_wrap, Ws}) ->
    Ws:send(Data).

send_term(Data, {comfy_socket_wrap, Ws}) ->
    Ws:send(comfy_web_utils:term_to_string(Data)).
