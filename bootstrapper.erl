-module(bootstrapper).
-export([start/0]).

start() ->
    start_logger(),
    application:start(misultin),
    application:start(socketio),
    application:start(comfy_server),
    application:start(comfy_web),
    sync:go().

start_logger() ->
    application:start(log4erl),
    error_logger:delete_report_handler(error_logger),
    error_logger:tty(false),
    log4erl:conf("/home/mefcorvi/projects/comfy/private/log4erl.conf"),
    log4erl:error_logger_handler().
