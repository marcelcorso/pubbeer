%% @author author <marcel>
%% @copyright YYYY author.

%% @doc Web server for pubbeer.

-module(pubbeer_web).
-author('marcel').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

% GET /
% POST /
% GET /node_name
% DELETE /node_name
% GET /node_name/activities
% POST /node_name/activities
% GET /node_name/activities/31415
% DELETE /node_name/activities/31415
% GET /node_name/subscribers
% POST /nodename/subscribers
% GET /node_name/subscribers/31415
% DELETE /node_name/subscribers/31415

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            erlang:display("Path: -" ++ Path ++ "-"),
            case Path of
                "" ->
                  Nodes = pubbeer_core:list_nodes(),
                  Template = lists:foldl(fun(_, Acc) -> ["~s~n"|Acc] end, [], Nodes),
                  success(Req, subst(lists:flatten(Template), Nodes));
%                Node ++ "/activities" -> 
%                  Activities = pubbeer_core:activities(Node),
%                  Template = lists:foldl(fun(_, Acc) -> ["~s~n"|Acc] end, [], Activities),
%                  success(Req, subst(lists:flatten(Template), Activities));
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                %"" -> 
                 
                  %Json = proplists:get_value("json", Data),
                  %Struct = mochijson2:decode(Json),

                  %%io:format("~nStruct : ~p~n", [Struct]),

                  %Name = struct:get_value(<<"name">>, Struct),

                  %Result = pubbeer_core:create(Name),

                  %%io:format("~nResult : ~p~n", [Result]),

                  %DataOut = mochijson2:encode(Result),

                  %Req:ok({"application/json", [], [DataOut]});

                _ -> Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


%error(Req, Body) when is_binary(Body) ->
%  Req:respond({500, [{"Content-Type", "text/plain"}], Body}).

success(Req, Body) when is_binary(Body) ->
  Req:respond({200, [{"Content-Type", "text/plain"}], Body}).

subst(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).




%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
