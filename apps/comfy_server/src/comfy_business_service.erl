-module(comfy_business_service).
-export([create_new_user/3, authorize/3, start_user_session/3]).

create_new_user(Server, Login, Password) ->
    DbName = get_db_name(Login),
    {ok, Db} = couchbeam:create_db(Server, DbName),
    couchbeam:save_doc(Db, {[
			     {<<"_id">>, <<"_design/default">>},
			     {<<"language">>, <<"javascript">>},
			     {<<"views">>, {[
					     {<<"tasks">>, get_tasks_view()}
					    ]}
			     }
			    ]}),
    {ok, user_registered}.

authorize(Server, Login, Password) ->
    case couchbeam:db_exists(Server, get_db_name(Login)) of
	true ->
	    true;
	false ->
	    false
    end.

start_user_session(Server, Login, Password) ->
    DbName = get_db_name(Login),
    {ok, Db} = couchbeam:open_db(Server, DbName),
    {ok, NewPid} = comfy_server_session:start_link(Login, Password),
    NewPid.
    
get_tasks_view() ->
    {[
      {<<"map">>, <<"function(doc) { emit(doc); }">>}
     ]}.   

get_db_name(Login) ->
    "user_" ++ string:to_lower(Login).
