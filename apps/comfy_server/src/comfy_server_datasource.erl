-module(comfy_server_datasource).
-export([start_link/2]).

-record(state, {
	  db :: any(),
	  viewName :: string(),
	  parentPid :: pid()
	 }).

start_link(Db, ViewName) ->
    SelfPid = self(),
    Pid = spawn_link(fun() -> init(#state{db=Db, viewName=ViewName, parentPid=SelfPid}) end),
    couchbeam_changes:stream(Db, Pid, [continuous, include_docs, heartbeat, {filter, "default/"++ViewName}]),
    {ok, Pid}.

init(#state{db=Db,viewName=ViewName,parentPid=ParentPid}=State) ->
    gen_server:cast(ParentPid, {datasource_loaded, self()}),
    loop(State).

loop(#state{db=Db,viewName=ViewName,parentPid=ParentPid}=State) ->
    receive
	stop ->
	    ok;
	{error, LastSeq, Msg} ->
	    gen_server:cast(ParentPid, {datasource_error, self()});	    
	{change, StartRef, Row} ->
	    gen_server:cast(ParentPid, {datasource_updated, self(), Row}),
	    loop(State);
	_ ->
	    loop(State)
    end.
