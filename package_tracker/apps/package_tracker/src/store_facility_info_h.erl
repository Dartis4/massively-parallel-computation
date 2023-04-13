-module(store_facility_info_h).

-export([init/2]).

init(Req0, Opts) ->
  io:fwrite("Store Facility~n"),
  {ok, Data, _} = cowboy_req:read_body(Req0),
  Dict = jsx:decode(Data),
  Worker = store_facility_dispatcher:iterate_worker(store_facility_dispatcher),
  Result = jsx:encode(store_facility_info_server:store_facility_info(Worker, Dict)),
  Req = cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/json">>
     }, Result, Req0),
  {ok, Req, Opts}.
