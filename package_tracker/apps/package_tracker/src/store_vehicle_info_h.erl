-module(store_vehicle_info_h).

-export([init/2]).

init(Req0, Opts) ->
  io:fwrite("Store Vehicle~n"),
  {ok, Data, _} = cowboy_req:read_body(Req0),
  Dict = jsx:decode(Data),
  Worker = store_vehicle_dispatcher:iterate_worker(store_vehicle_dispatcher),
  Result = jsx:encode(store_vehicle_info_server:store_vehicle_info(Worker, Dict)),
  Req = cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/json">>
     }, Result, Req0),
  {ok, Req, Opts}.
