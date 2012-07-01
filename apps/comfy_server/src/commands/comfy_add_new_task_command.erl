-module(comfy_add_new_task_command).
-export([handle/2]).
-include("comfy_server.hrl").

handle(TaskData, #session_state{db=Db}=State) ->
    ?Log("TaskData 2: ~p~n", [TaskData]),
    couchbeam:save_doc(Db, {[
			     {type, task}
			    ] ++ TaskData}),
    {{ok, task_created}, State}.

