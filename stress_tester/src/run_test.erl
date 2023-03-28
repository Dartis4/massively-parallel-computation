-module(run_test).

-export([single_package/0, ten_package/0]).

single_package() ->
  stresser:start_all("server.dartis.dev", tester, 1).

ten_package() ->
  stresser:start_all("server.dartis.dev", tester, 10).
