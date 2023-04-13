-module(test).

-export([single_package/0, ten_package/0]).


single_package() ->
  io:fwrite("Sending One Package~n"),
  stresser:start_all("server.dartis.dev", tester, 1),
  io:fwrite("Sent~n").

ten_package() ->
  io:fwrite("Sending Ten Packages~n"),
  stresser:start_all("server.dartis.dev", tester, 10),
  io:fwrite("Sent~n").
