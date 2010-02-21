-module(test_activity_store).

-include_lib("eunit/include/eunit.hrl").

server_test_() ->
  {setup, fun() -> activity_store:start_link() end,
   fun(_Pid) -> activity_store:shutdown() end,
   fun generate_tests/1}.


generate_tests(_Pid) ->
  [ fun() ->
      {Result, _} = activity_store:get("doc1"),
      ?assertEqual(Result, false)
    end,
    fun() ->
      ?assert(activity_store:push("doc2", "activity1")),
      {Result, Doc} = activity_store:get("doc2"),
      ?assertEqual(Result, true),
      ?assertEqual(Doc, ["activity1"])
    end,
    fun() ->
      ?assert(activity_store:push("doc3", "activity1")),
      ?assert(activity_store:push("doc3", "activity2")),
      {Result, Doc} = activity_store:get("doc3"),
      ?assertEqual(Result, true),
      ?assertEqual(Doc, ["activity2", "activity1"])
    end
    ].
