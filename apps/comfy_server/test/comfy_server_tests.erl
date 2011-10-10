-module(comfy_server_tests).
-compile(export_all).
-include("comfy_server.hrl").
-include_lib("eunit/include/eunit.hrl").

comfy_server_test_() ->
    {setup, fun global_setup/0, fun global_teardown/1,
     [{foreach, fun setup/0, fun teardown/1,
       [{with, [T]} || T <- [
			     fun call_unknown_command/1,
			     fun cast_unknown_command/1,
			     fun send_info_message/1
			    ]]}
     ]}.

global_setup() ->
    ok.
    
global_teardown(_) ->
    ok.

setup() ->
    ok.
    
teardown(_) ->
    ok.

call_unknown_command(_) ->
    ok.
%    ?assertEqual(unknown_command, gen_server:call(Pid, {some_unknown_command})).

cast_unknown_command(_) ->
    ok.
%    gen_server:cast(Pid, {some_unknown_command}).

send_info_message(_) ->
    ok.
%    Pid ! message.
