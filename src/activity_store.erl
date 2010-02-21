-module(activity_store).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, get/1, push/2, shutdown/0]).


-include_lib("stdlib/include/qlc.hrl").


-record(timeline,
  {
   node_name
   activities,
   updated_on}).


%% Client functions
start_link() ->
  init_store(),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
  
get(DocName) ->
  gen_server:call(?MODULE, {get, NodeName}).

push(NodeName, Activity) ->
  gen_server:call(?MODULE, {push, NodeName, Activity}).

shutdown() ->
  gen_server:cast(?MODULE, stop).


%% gen_server functions

init([]) ->
  inets:start(),
  {ok, false}.

handle_call({get, NodeName}, _From, _State) ->


  case dict:find(NodeName, Nodes) ofdd
    {ok, _} ->
      {reply, false, Nodes};
    error ->
      {reply, ok, dict:store(NodeName, [], Nodes)}
  end;

handle_call({push, NodeName, Activity}, _From, Nodes) ->

  F = fun() ->
    {_, UpdatedOn, _} = erlang:now(),
    mnesia:write(#timeline{node_name=NodeName, activities=NewActivities, updated_on=UpdatedOn}) end,
  mnesia:transaction(F).


 
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(stop, State) ->
  inets:stop(),
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  io:format("Info message received: ~p~n", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("Server is stopping...~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


% private functions
init_store() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  try
    mnesia:table_info(timeline, type)
  catch
    exit: _ ->
      mnesia:create_table(timeline, [{attributes, record_info(fields, timeline)},
           {type, bag},
           {disc_copies, [node()]}])
  end.


