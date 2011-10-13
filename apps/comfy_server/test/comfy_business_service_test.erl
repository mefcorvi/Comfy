-module(comfy_business_service_test).
-compile(export_all).
-include("comfy_server.hrl").
-include_lib("eunit/include/eunit.hrl").

comfy_business_service_test_() ->
    {setup, fun global_setup/0, fun global_teardown/1,
     [{foreach, fun setup/0, fun teardown/1,
       [{with, [T]} || T <- [
			     fun create_new_user/1
			    ]]}
     ]}.

global_setup() ->
    ok.
    
global_teardown(_) ->
    ok.

setup() ->
    meck:new(couchbeam),
    ok.

teardown(_) ->
    meck:unload(couchbeam),
    ok.

create_new_user(_) ->
    Host = comfy_server_config:get(db_host),
    Port = comfy_server_config:get(db_port),
    ServerConnection = {server_connection},
    Db = {db},
    meck:expect(couchbeam, server_connection, fun(_, _) -> ServerConnection end),
    meck:expect(couchbeam, db_exists, fun(_, "user_testlogin") -> false end),
    meck:expect(couchbeam, create_db, fun(_, "user_testlogin") -> {ok, Db} end),
    meck:expect(couchbeam, save_doc, fun(_, _) -> ok end),
    ?assertEqual({ok, user_registered}, comfy_business_service:create_new_user("TestLogin", "TestPwd")).
    
