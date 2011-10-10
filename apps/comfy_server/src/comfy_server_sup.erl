-module(comfy_server_sup).
-behaviour(supervisor).
-include("comfy_server.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    ?Log("Starting supervisor..."),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ?Log("Supervisor started"),
    {ok, { {one_for_one, 5, 10}, [
				  ?CHILD(comfy_server, worker)
				 ]}
    }.
