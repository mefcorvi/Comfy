-module(comfy_web_utils).
-export([parse_term/1, term_to_string/1]).

parse_term(Data) ->
    case erl_scan:string(Data) of
	{ok, Tokens, _} ->
	    case erl_parse:parse_term(Tokens) of
		{ok, Term} -> {ok, Term};
		_ -> {wrong_term, Data}
	    end;
	_ -> {wrong_term, Data}
    end.

term_to_string(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).
