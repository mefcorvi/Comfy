-module(comfy_add_new_task_command).
-export([handle/2]).
-include("comfy_server.hrl").

handle(TaskData, #session_state{db=Db}=State) ->
    ?Log("TaskData: ~p~n", [TaskData]),
    couchbeam:save_doc(Db, {[
			     {type, task},
			     {name, proplists:get_value(<<"name">>, TaskData)}
			    ]}),
    {{ok, task_created}, State}.

