-module(comfy_web_server).
-export([start/1, stop/0]).
-include("comfy_web_log.hrl").
-include_lib("kernel/include/file.hrl").

start(Port) ->
    ?Log("Starting web server at ~p port~n", [Port]),
    misultin:start_link([
			 {port, Port},
			 {compress, true},
			 {loop, fun(Req) -> handle_http(Req) end},
			 {ws_loop, fun(Ws) -> process_socket(comfy_socket_wrap:wrap(Ws)) end}
			]).

stop() ->
    misultin:stop().

handle_http(Req) ->	
    %% output
    {abs_path, Uri} = Req:get(uri),
    handle(Req, Uri).

handle(Req, "/") ->
    handle_file(Req, "/index.html");

handle(Req, "/scripts.js") ->
    handle_file(Req, "/scripts.js");

handle(Req, Uri) ->
    handle_file(Req, Uri).

handle_file(Req, FileName) ->
    ContentType = case filename:extension(FileName) of
		      ".js" -> "text/javascript";
		      ".css" -> "text/css";
		      _ -> "text/html"
		  end,
    FilePath = filename:join([comfy_web_config:get(docroot) ++ FileName]),
    case file:read_file_info(FilePath) of
	{ok, FileInfo} ->
	    ModifyTime = httpd_util:rfc1123_date(FileInfo#file_info.mtime),
	    Etag = httpd_util:create_etag(FileInfo),
	    case get_header(Req, if_none_match) of
		Etag ->
		    Req:respond(304);
		_ ->
		    Req:file(FilePath, [
					{'Content-Type', ContentType},
					{'Cache-Control', "public"},
					{'Last-Modified', ModifyTime},
					{'ETag', Etag}
				       ])
	    end;
	_ ->
	    Req:respond(404, [{"Content-Type", "text/html"}], "File not found")
    end.

get_header(Req, if_none_match) ->
    Headers = Req:get(headers),
    misultin_utility:get_key_value('If-None-Match', Headers);

get_header(Req, if_modified_since) ->
    Headers = Req:get(headers),
    misultin_utility:get_key_value('If-Modified-Since', Headers).
    
%%--------------------------------------------------------------------
%% @doc Запускает цикл обработки нового веб-сокета.
%% @end
%%--------------------------------------------------------------------
-spec process_socket(term()) -> ok.
process_socket(Ws) ->
    process_flag(trap_exit, true),
    Sid = start_session(Ws),
    handle_websocket(Ws, Sid).

%%--------------------------------------------------------------------
%% @doc Запускает новую сессию
%% @end
%%--------------------------------------------------------------------
-spec start_session(term()) -> node().
start_session(Ws) ->
    comfy_web_session:start(Ws).

%%--------------------------------------------------------------------
%% @doc Цикл обработки сообщений веб-сокета
%% @spec handle_websocket(term(), node()) -> no_return()
%% @end
%%--------------------------------------------------------------------
handle_websocket(Ws, Sid) ->
    receive
	{browser, Data} ->
	    TermString = string:concat(Data, "."),
	    case comfy_web_utils:parse_term(TermString) of
		{ok, Term} ->
		    Sid ! Term;
		Error ->
		    Ws:send_term({err, Error})
	    end,
	    handle_websocket(Ws, Sid);
	{'EXIT', Sid, _Reason} -> % сессия погибла, перезапустим её
	    Ws:send_term({session_died}),
	    ?Error("Session ~p died because ~p", [Sid, _Reason]),
            handle_websocket(Ws, start_session(Ws));
	_Ignore ->
	    handle_websocket(Ws, Sid)
    end.
