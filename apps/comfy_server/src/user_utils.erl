-module(user_utils).
-export([get_db_name/1]).

get_db_name(Login) ->
    "user_" ++ string:to_lower(Login).
