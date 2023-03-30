-module(store_package_info_h).

-export([init/2]).

init(Req0, Opts) ->
  io:fwrite("Store Package handler~n"),
  {ok, Data, _} = cowboy_req:read_body(Req0),
  Dict = jsx:decode(Data),
  Worker = store_package_dispatcher:iterate_worker(store_package_dispatcher),
  Result = jsx:encode(store_package_info_server:store_package_info(Worker, Dict)),
  Req = cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/json">>
     }, Result, Req0),
  {ok, Req, Opts}.
