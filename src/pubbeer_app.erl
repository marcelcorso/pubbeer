%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the pubbeer application.

-module(pubbeer_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for pubbeer.
start(_Type, _StartArgs) ->
    pubbeer_deps:ensure(),
    pubbeer_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for pubbeer.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
