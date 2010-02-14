-module(pubbeer_core_test).

-export([run/0]).

-import(pubbeer_core).



run() ->
  pubbeer_core:start_link(),
  pubbeer_core:create(marcel),
  pubbeer_core:sub(marcel, "http://localhost:3000/plays"),
  pubbeer_core:sub(marcel, "http://localhost:3000/favorites"),
  pubbeer_core:pub(marcel, "play[artist]=pavaka&play[title]=dreamers").
