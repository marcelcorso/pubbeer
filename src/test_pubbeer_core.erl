-module(test_pubbeer_core).

-include_lib("eunit/include/eunit.hrl").

server_test_() ->
  {setup, fun() -> pubbeer_core:start_link() end,
   fun(_Pid) -> pubbeer_core:shutdown() end,
   fun generate_tests/1}.


generate_tests(_Pid) ->
  erlang:display("generating tests!"),
  [ fun() ->
      erlang:display("running test!"),
      pubbeer_core:create("node1"),
      ?assert(lists:member("node1", pubbeer_core:list_nodes()))
    end,
    fun() ->
      pubbeer_core:create("node2"),
      pubbeer_core:destroy("node2"),
      ?assertNot(lists:member("node2", pubbeer_core:list_nodes()))
    end,
    fun() ->
      pubbeer_core:create("node3"),
      pubbeer_core:sub("node3", "http://pubbeer.org"),
      ?assert(lists:member("http://pubbeer.org", pubbeer_core:list_subscribers("node3")))
    end,
    fun() ->
      pubbeer_core:create("node4"),
      pubbeer_core:sub("node4", "http://pubbeer.org"),
      pubbeer_core:unsub("node4", "http://pubbeer.org"),
      ?assertNot(lists:member("http://pubbeer.org", pubbeer_core:list_subscribers("node4")))
    end,
    fun() ->
      pubbeer_core:create("node5"),
      pubbeer_core:sub("node5", "http://pubbeer.org"),
      Data = "data=123",
      pubbeer_core:pub("node5", Data),
      [Head | Tail] = pubbeer_core:activities("node5"),
      ?assertEqual(Head, Data),
      ?assertEqual(Tail, [])
    end,
    fun() ->
      ?assert(false)
    end
    ].

