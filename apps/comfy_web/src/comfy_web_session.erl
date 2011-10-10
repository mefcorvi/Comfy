-module(comfy_web_session).
-export([start/1]).

start(Ws) ->
    spawn_link(fun() -> init(Ws) end).

init(Ws) ->
    loop(Ws, comfy_web_config:get(data_service_pid)).

call(Pid, Message) ->
    gen_server:call(Pid, Message).

loop(Ws, Pid) ->
    receive
	Msg ->
	    Reply = call(Pid, Msg),
	    case Reply of
		{new_server, NewPid} ->
		    Ws:send_term(call(NewPid, Msg)),
		    loop(Ws, NewPid);
		_ ->
		    Ws:send_term(Reply),
		    loop(Ws, Pid)
	    end
    end.
