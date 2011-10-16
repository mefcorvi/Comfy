-module(comfy_web_session).
-export([start/1]).
-include("comfy_web_log.hrl").

start(Ws) ->
    spawn_link(fun() -> init(Ws) end).

init(Ws) ->
    loop(Ws, comfy_web_config:get(data_service_pid)).

call(Pid, {add_new_task, TaskJson}) ->
    {TaskData} = ejson:decode(TaskJson),
    gen_server:call(Pid, {add_new_task, TaskData});

call(Pid, get_tasks) ->
    TasksList = gen_server:call(Pid, get_tasks),
    ejson:encode(TasksList);

call(Pid, {login, Login, Password}) ->
    gen_server:call(Pid, {login, Login, Password});

call(Pid, logout) ->
    {ok, logout} = gen_server:call(Pid, logout),
    {ok, logout, comfy_web_config:get(data_service_pid)};

call(Pid, Msg) ->
    {error, cannot_handle, Msg}.

loop(Ws, Pid) ->
    receive
	Msg ->
	    try
		Reply = call(Pid, Msg),
		case Reply of
		    {ok, user_authorized, NewPid} ->
			Ws:send_term({ok, user_authorized}),
			loop(Ws, NewPid);
		    {ok, logout, NewPid} ->
			Ws:send_term({ok, logout}),
			loop(Ws, NewPid);
		    Bin when is_binary(Bin) ->
			Ws:send(Bin),
			loop(Ws, Pid);
		    _ ->
			Ws:send_term(Reply),
			loop(Ws, Pid)
		end
	    catch
		_ ->
		    Ws:send_term({error, internal_error, Msg}),
		    loop(Ws, Pid)
	    end
    end.
