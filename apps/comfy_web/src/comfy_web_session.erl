-module(comfy_web_session).
-export([start/1]).
-include("comfy_web_log.hrl").

start(Ws) ->
    spawn_link(fun() -> init(Ws) end).

init(Ws) ->
    loop(Ws, comfy_web_config:get(data_service_pid)).

%% Добавление новой таски
call(Pid, {command, add_new_task, TaskJson}) ->
    {TaskData} = ejson:decode(TaskJson),
    gen_server:call(Pid, {command, add_new_task, TaskData});

%% Создание нового датасурса
call(Pid, {create_datasource, ViewName}) ->
    DataSourceId = gen_server:call(Pid, {create_datasource, ViewName});

%% Удаление датасурса
call(Pid, {drop_datasource, DataSourceId}) ->
    gen_server:call(Pid, {drop_datasource, DataSourceId});

%% Логин пользователя
call(Pid, {login, Login, Password}) ->
    gen_server:call(Pid, {login, Login, Password});

%% Логаут пользователя
call(Pid, logout) ->
    {ok, logout} = gen_server:call(Pid, logout),
    {ok, logout, comfy_web_config:get(data_service_pid)};

call(Pid, Msg) ->
    {error, ?MODULE, cannot_handle, Msg}.

loop(Ws, Pid) ->
    receive
	{Pid, Msg} ->
	    case Msg of
		{datasource_loaded, DataSourceId} ->
		    Ws:send_term({datasource_loaded, DataSourceId}),
		    loop(Ws, Pid);
		{datasource_updated, DataSourceId, Row} ->
		    Ws:send_term({datasource_updated, DataSourceId, ejson:encode(Row)}),
		    loop(Ws, Pid);
		_Other ->
		    ?Log("[Comfy Web Session] Wrong command: ~p~n", [_Other])
	    end;
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
