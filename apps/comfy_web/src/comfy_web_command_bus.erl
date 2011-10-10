-module(comfy_web_command_bus).
-export([send_command/2, receive_event/1]).

send_command(Name, Args) when is_atom(Name) ->
    gen_server:call(comfy_web_config:get(data_service_pid), {command, Name, Args}).

receive_event(EventType) ->
    DataServicePid = comfy_web_config:get(data_service_pid),
    TimeOut = comfy_web_config:get(data_service_timeout),
    receive
	{DataServicePid, event, EventType, EventData} ->
	    EventData
    after TimeOut ->
	    {error, timeout_experied}
    end.
