-module(comfy_server_session).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  db :: any()
	 }).

start_link(Login, Password) ->
    gen_server:start_link(?MODULE, {Login, Password}, []).

init({Login, Password}) ->
    {ok, #state{}}.

handle_call({login, Login, Pwd}, _From, State) ->
    {reply, {ok, user_authorized}, State};

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

