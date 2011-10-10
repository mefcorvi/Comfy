-module(comfy_server).
-behaviour(gen_server).
-include("comfy_server.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  db_server :: any()
	 }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, comfy_server}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Server = couchbeam:server_connection(comfy_server_config:get(db_host), comfy_server_config:get(db_port)),
    {ok, #state{
       db_server = Server
      }}.

handle_call({login, Login, Pwd}, From, #state{db_server=Server}=State) when is_list(Login) ->
    case comfy_business_service:authorize(Server, Login, Pwd) of
	true ->
	    NewPid = comfy_business_service:start_user_session(Server, Login, Pwd),
	    {reply, {new_server, NewPid}, State};
	false ->
	    {reply, {error, wrong_login_or_password}, State}
    end;

handle_call({register, Login, Pwd}, From, #state{db_server=Server}=State) when is_list(Login) ->
    {reply, comfy_business_service:create_new_user(Server, Login, Pwd), State};

handle_call(Request, _From, State) ->
    Reply = {error, wrong_request, Request},
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
