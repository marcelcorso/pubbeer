-module(pubbeer_test).

-export([run/0]).

-import(pubbeer).



run() ->
  pubbeer:start_link(),
  pubbeer:create(marcel),
  pubbeer:sub(marcel, "http://localhost:3000/plays"),
  pubbeer:sub(marcel, "http://localhost:3000/favorites"),
  pubbeer:pub(marcel, "play[artist]=pavaka&play[title]=dreamers").