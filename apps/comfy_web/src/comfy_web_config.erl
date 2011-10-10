-module(comfy_web_config).
-export([get/1]).

-define(RootDir, "/home/mefcorvi/projects/comfy/apps/comfy_web/").
-define(RootDir(Folder), "/home/mefcorvi/projects/comfy/apps/comfy_web/" ++ Folder).

get(docroot) ->
    ?RootDir("static/");

get(data_service_pid) ->
    {comfy_server, 'comfy@127.0.0.1'};

get(data_service_timeout) ->
    5000;

get(_) ->
    undefined.
