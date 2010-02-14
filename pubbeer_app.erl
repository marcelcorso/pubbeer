%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the ../../pubbeer/pubbeer application.

-module(../../pubbeer/pubbeer_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ../../pubbeer/pubbeer.
start(_Type, _StartArgs) ->
    ../../pubbeer/pubbeer_deps:ensure(),
    ../../pubbeer/pubbeer_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ../../pubbeer/pubbeer.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
