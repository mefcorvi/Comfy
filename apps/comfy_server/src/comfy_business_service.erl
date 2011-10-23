-module(comfy_business_service).
-export([create_new_user/2, remove_user/2, authorize/2, start_user_session/3]).
-include("comfy_server.hrl").

create_new_user(Login, Password) ->
    Server = connect_to_master_db(),
    DbName = user_utils:get_db_name(Login),
    case couchbeam:db_exists(Server, DbName) of
	true ->
	    {error, user_already_registered_with_same_login};
	false ->
	    {ok, Db} = couchbeam:create_db(Server, DbName),
	    couchbeam:save_doc(Db, {[
				     {<<"_id">>, <<"_design/default">>},
				     {language, javascript},
				     {views, {[
					       {tasks, get_tasks_view()}
					      ]}
				     }
				    ]}),
	    {ok, user_registered}
    end.

remove_user(Login, Password) ->
    Server = connect_to_master_db(),
    DbName = user_utils:get_db_name(Login),
    case couchbeam:db_exists(Server, DbName) of
	true ->
	    {ok, _} = couchbeam:delete_db(Server, DbName),
	    {ok, user_removed};
	false ->
	    {ok, user_is_not_registered}
    end.    

authorize(Login, Password) ->
    Server = connect_to_master_db(),
    case couchbeam:db_exists(Server, user_utils:get_db_name(Login)) of
	true ->
	    true;
	false ->
	    false
    end.

start_user_session(Login, Password, SessionPid) ->
    comfy_server_session:start_link(Login, Password, SessionPid).
    
get_tasks_view() ->
    {[
      {map, <<"function(doc) { if (doc.type == 'task') emit(doc._id, doc); }">>}
     ]}.

connect_to_master_db() ->
    couchbeam:server_connection(comfy_server_config:get(db_host), comfy_server_config:get(db_port)).
