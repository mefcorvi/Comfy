-module(comfy_server_session).
-behaviour(gen_server).
-include("comfy_server.hrl").

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Создаёт новую сессию
start_link(Login, Password, SessionPid) ->
    gen_server:start_link(?MODULE, {Login, Password, SessionPid}, []).

%% Инициализирует новую сессию
init({Login, Password, ClientPid}) ->
    Server = connect_to_user_db(),
    DbName = user_utils:get_db_name(Login),
    {ok, Db} = couchbeam:open_db(Server, DbName),
    {ok, #session_state{
       db=Db,
       client=ClientPid,
       dataSources=[]}
    }.

%% Логаут пользователя
handle_call(logout, _From, State) ->
    {stop, normal, {ok, logout}, State};

%% Хэндлинг комманд
handle_call({command, CommandName, Args}, _From, State) ->
    ModuleName = list_to_atom(lists:append(["comfy_", atom_to_list(CommandName), "_command"])),
    {Reply, NewState} = erlang:apply(ModuleName, handle, [Args, State]),
    {reply, Reply, NewState};

%% Создаёт новый датасурс
handle_call({create_datasource, ViewName}, _From, State) ->
    Db = State#session_state.db,
    {ok, Pid} = comfy_server_datasource:start_link(Db, ViewName),
    DataSourceId = erlang:phash2({node(), now()}),
    NewDataSources = State#session_state.dataSources ++ [{DataSourceId, Pid}],
    {reply, DataSourceId, State#session_state{dataSources=NewDataSources}};

%% Дропает старый датасурс
handle_call({drop_datasource, DataSourceId}, _From, State) ->
    case proplists:lookup(DataSourceId, State#session_state.dataSources) of
	none ->
	    {reply, {error, datasource_not_found}, State};
	{DataSourceId, DataSourcePid} ->
	    DataSourcePid ! stop,
	    NewDataSources = proplists:delete(DataSourceId, State#session_state.dataSources),
	    {reply, {ok, datasource_dropped, DataSourceId}, State#session_state{dataSources=NewDataSources}}
    end;

%% Убивает сессию
handle_call(stop, _From, State) ->
    {reply, stop, normal, State};

%% Неверный запрос
handle_call(Request, _From, State) ->
    {reply, {error, wrong_request, Request}, State}.

%% Получили инфу от датасурса о том, что он загружен
handle_cast(Msg={datasource_loaded, From}, State) ->
    send_message(self(), Msg, State),
    {noreply, State};

%% Получили инфу от датасурса о том, что он изменен
handle_cast(Msg={datasource_updated, From, Row}, State) ->
    send_message(self(), Msg, State),
    {noreply, State};

%% Получили инфу от датасурса о том, что он изменен
handle_cast(Msg={datasource_error, From}, State) ->
    send_message(self(), Msg, State),
    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {?MODULE, unknown_message, Msg}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal

%% Отправляет сообщение клиенту
send_message(Pid, Msg, State) ->
    {ClientPid, _} = State#session_state.client,
    InnerMsg = {Pid, Msg},
    ClientPid ! InnerMsg.    

connect_to_user_db() ->
    couchbeam:server_connection(comfy_server_config:get(db_host), comfy_server_config:get(db_port)).
