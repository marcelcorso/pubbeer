%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(../../pubbeer/pubbeer).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the ../../pubbeer/pubbeer server.
start() ->
    ../../pubbeer/pubbeer_deps:ensure(),
    ensure_started(crypto),
    application:start(../../pubbeer/pubbeer).

%% @spec stop() -> ok
%% @doc Stop the ../../pubbeer/pubbeer server.
stop() ->
    Res = application:stop(../../pubbeer/pubbeer),
    application:stop(crypto),
    Res.
