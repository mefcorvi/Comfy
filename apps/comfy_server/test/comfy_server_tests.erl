-module(comfy_server_tests).
-compile(export_all).
-include("comfy_server.hrl").
-include_lib("eunit/include/eunit.hrl").

comfy_server_test_() ->
    {setup, fun global_setup/0, fun global_teardown/1,
     [{foreach, fun setup/0, fun teardown/1,
       [{with, [T]} || T <- [
			     fun user_success_login/1,
			     fun user_fail_login/1,
			     fun user_register/1,
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
    meck:new(comfy_business_service),
    {ok, Pid} = comfy_server:start_link(),
    Pid.
    
teardown(Pid) ->
    gen_server:call(Pid, stop),
    meck:unload(comfy_business_service),
    ok.

user_success_login(Pid) ->
    meck:expect(comfy_business_service, authorize, fun("user_name", "user_pwd") -> true end),
    meck:expect(comfy_business_service, start_user_session, fun("user_name", "user_pwd") -> new_pid end),
    ?assertEqual({new_server, new_pid}, gen_server:call(Pid, {login, "user_name", "user_pwd"})).

user_fail_login(Pid) ->
    meck:expect(comfy_business_service, authorize, fun("user_name", "user_pwd") -> false end),
    ?assertEqual({error, wrong_login_or_password}, gen_server:call(Pid, {login, "user_name", "user_pwd"})).

user_register(Pid) ->
    meck:expect(comfy_business_service, create_new_user, fun("user_name", "user_pwd") -> {ok, user_registered} end),
    ?assertEqual({ok, user_registered}, gen_server:call(Pid, {register, "user_name", "user_pwd"})).

call_unknown_command(Pid) ->
    ?assertEqual({error, wrong_request, some_unknown_command}, gen_server:call(Pid, some_unknown_command)).

cast_unknown_command(Pid) ->
    ?assertEqual(ok, gen_server:cast(Pid, some_unknown_command)).

send_info_message(Pid) ->
    Pid ! message.
