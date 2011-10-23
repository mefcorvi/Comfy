-module(comfy_server_session).
-behaviour(gen_server).
-include("comfy_server.hrl").

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  db :: any(),
	  client :: pid()
	 }).

start_link(Login, Password, SessionPid) ->
    gen_server:start_link(?MODULE, {Login, Password, SessionPid}, []).

init({Login, Password, ClientPid}) ->
    Server = connect_to_user_db(),
    DbName = user_utils:get_db_name(Login),
    {ok, Db} = couchbeam:open_db(Server, DbName),
    {ok, #state{db=Db,client=ClientPid}}.

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

handle_call({create_datasource, ViewName}, _From, State) ->
    Db = State#state.db,
    {ok, Pid} = comfy_server_datasource:start_link(Db, ViewName),
    {reply, Pid, State};

handle_call(stop, _From, State) ->
    {reply, stop, normal, State};

handle_call(Request, _From, State) ->
    {reply, {error, wrong_request, Request}, State}.

handle_cast({datasource_loaded, From, Rows}, State) ->
    {ClientPid, _} = State#state.client,
    Msg = {self(), {datasource_loaded, From, Rows}},
    io:format("Client Pid: ~p~n", [ClientPid]),
    ClientPid ! Msg,
    {noreply, State};

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
