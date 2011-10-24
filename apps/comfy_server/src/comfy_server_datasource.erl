-module(comfy_server_datasource).
-export([start_link/2]).

-record(state, {
	  db :: any(),
	  viewName :: string(),
	  parentPid :: pid()
	 }).

start_link(Db, ViewName) ->
    SelfPid = self(),
    {ok, spawn_link(fun() -> init(#state{db=Db, viewName=ViewName, parentPid=SelfPid}) end)}.

init(#state{db=Db,viewName=ViewName,parentPid=ParentPid}=State) ->
    {ok, Rows} = couchbeam_view:fetch(Db, {"default", ViewName}),
    gen_server:cast(ParentPid, {datasource_loaded, self(), Rows}),
    loop(State).

loop(State) ->
    receive
	stop ->
	    ok;
	_ -> loop(State)
    end.
