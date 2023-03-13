-module(store_facility_info_h).

-export([init/2]).

init(Req0, Opts) ->
  {ok, Data, _} = cowboy_req:read_body(Req0),
  [Item|_] = jsx:decode(Data),
  Result = jsx:encode(store_facility_info_server:store_facility_info(Item)),
  Req = cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/json">>
     }, Result, Req0),
  {ok, Req, Opts}.
