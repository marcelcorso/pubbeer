-module(pubbeer_core).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, create/1, destroy/1, pub/2, sub/2, unsub/2, list_nodes/0, list_subscribers/1, shutdown/0]).

%% Client functions
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
  
create(NodeName) ->
  gen_server:call(?MODULE, {create, NodeName}).

destroy(NodeName) ->
  gen_server:call(?MODULE, {destroy, NodeName}).

list_nodes() ->
  gen_server:call(?MODULE, {list_nodes}).

pub(NodeName, Body) ->
  gen_server:call(?MODULE, {pub, NodeName, Body}).

sub(NodeName, Subscriber) ->
  gen_server:call(?MODULE, {sub, NodeName, Subscriber}).

unsub(NodeName, Subscriber) ->
  gen_server:call(?MODULE, {unsub, NodeName, Subscriber}).

list_subscribers(NodeName) -> 
  gen_server:call(?MODULE, {list_subscribers, NodeName}).

shutdown() ->
  gen_server:cast(?MODULE, stop).


%% gen_server functions

init([]) ->
  inets:start(),
  {ok, dict:new()}.

handle_call({create, NodeName}, _From, Nodes) ->
  case dict:find(NodeName, Nodes) of
    {ok, _} ->
      {reply, false, Nodes};
    error ->
      {reply, ok, dict:store(NodeName, [], Nodes)}
  end;

handle_call({destroy, NodeName}, _From, Nodes) ->
  case dict:find(NodeName, Nodes) of
    {ok, _} ->
      {reply, ok, dict:erase(NodeName, Nodes)};
    error ->
      {reply, false, Nodes}
  end;
 
handle_call({list_nodes}, _From, Nodes) ->
  {reply, dict:fetch_keys(Nodes), Nodes};

 
handle_call({pub, NodeName, Body}, _From, Nodes) ->
  case dict:find(NodeName, Nodes) of
    {ok, NodeListeners} ->
      
      Notifier = fun(Listener) ->
        http:request(post, 
                    {
                      Listener, 
                      [],
                      "application/x-www-form-urlencoded",
                      Body
                    }, 
                    [], 
                    [{sync, false}])
      end,
      lists:foreach(Notifier, NodeListeners),
      {reply, ok, Nodes};
    error ->
      {reply, false, Nodes}
  end;

handle_call({sub, NodeName, Subscriber}, _From, Nodes) ->
  case dict:find(NodeName, Nodes) of
    {ok, NodeListeners} ->
      case lists:member(Subscriber, NodeListeners) of 
        true ->
         {reply, ok, Nodes};
        false ->
          {reply, ok, dict:store(NodeName, [Subscriber|NodeListeners], Nodes)}
      end;
    error ->
      {reply, false, Nodes}
  end;  
  
handle_call({unsub, NodeName, Subscriber}, _From, Nodes) ->
  case dict:find(NodeName, Nodes) of
    {ok, NodeListeners} ->
      case lists:member(Subscriber, NodeListeners) of 
        true ->
          {reply, ok, dict:store(NodeName, lists:delete(Subscriber, NodeListeners), Nodes)};
        false ->
          {reply, false, Nodes}
      end;
    error ->
      {reply, false, Nodes}
  end;  
  
handle_call({list_subscribers, NodeName}, _From, Nodes) ->
  case dict:find(NodeName, Nodes) of
    {ok, NodeListeners} ->
      {reply, NodeListeners, Nodes};
    error ->
      {reply, false, Nodes}
  end;

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
