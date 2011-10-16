-module(comfy_server_session).
-behaviour(gen_server).
-include("comfy_server.hrl").

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  db :: any()
	 }).

start_link(Login, Password) ->
    gen_server:start_link(?MODULE, {Login, Password}, []).

init({Login, Password}) ->
    Server = connect_to_user_db(),
    DbName = user_utils:get_db_name(Login),
    {ok, Db} = couchbeam:open_db(Server, DbName),
    {ok, #state{db=Db}}.

handle_call(logout, _From, State) ->
    {stop, normal, {ok, logout}, State};

handle_call({add_new_task, TaskData}, _From, State) ->
    Db = State#state.db,
    ?Log("TaskData: ~p~n", [TaskData]),
    couchbeam:save_doc(Db, {[
			     {type, task},
			     {name, proplists:get_value(<<"name">>, TaskData)}
			    ]}),
    {reply, {ok, task_created}, State};

handle_call(get_tasks, _From, State) ->
    Db = State#state.db,
    {ok, Rows} = couchbeam_view:fetch(Db, {"default", "tasks"}),
    {reply, Rows, State};

handle_call(stop, _From, State) ->
    {reply, stop, normal, State};

handle_call(Request, _From, State) ->
    {reply, {error, wrong_request, Request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal

connect_to_user_db() ->
    couchbeam:server_connection(comfy_server_config:get(db_host), comfy_server_config:get(db_port)).
