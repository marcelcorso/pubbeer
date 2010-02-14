%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(pubbeer).
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
%% @doc Start the pubbeer server.
start() ->
    pubbeer_deps:ensure(),
    ensure_started(crypto),
    application:start(pubbeer).

%% @spec stop() -> ok
%% @doc Stop the pubbeer server.
stop() ->
    Res = application:stop(pubbeer),
    application:stop(crypto),
    Res.
