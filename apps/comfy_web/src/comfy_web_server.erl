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
    comfy_scripts:combine_scripts(),
    handle_file(Req, "/scripts.js");

handle(Req, "/styles.css") ->
    comfy_scripts:combine_styles(),
    handle_file(Req, "/styles.css");

handle(Req, "/pages") ->
    Args = Req:parse_qs(),
    PageName = Req:get_variable("pageUri", Args),
    handle_file(Req, "pages/" ++ PageName ++ ".js");

handle(Req, "/pages/getLists") ->
    Req:respond(200, [{'Content-Type', "application/json"}], "(["
		"{\"href\": \"page:default\", \"title\": \"Summary\", \"showInMenu\": true},"
		"{\"href\": \"page:settings\", \"title\": \"Settings\", \"showInMenu\": false},"
		"{\"href\": \"page:masters/default\", \"title\": \"Main page\", \"showInMenu\": false},"
		"{\"href\": \"page:errors/error404\", \"title\": \"Error 404\", \"showInMenu\": false},"
		"{\"href\": \"page:errors/error403\", \"title\": \"Error 403\", \"showInMenu\": false}"
		"])"),
    ok;

handle(Req, Uri) ->
    handle_file(Req, Uri).

handle_file(Req, FileName) ->
    ContentType = misultin_utility:get_content_type(FileName),
    FilePath = filename:join([comfy_web_config:get(docroot) ++ FileName]),
    case file:read_file_info(FilePath) of
	{ok, FileInfo} ->
	    Etag = httpd_util:create_etag(FileInfo),
	    case get_header(Req, if_none_match) of
		Etag ->
		    Req:respond(304);
		_ ->
		    ModifyTime = httpd_util:rfc1123_date(FileInfo#file_info.mtime),
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
