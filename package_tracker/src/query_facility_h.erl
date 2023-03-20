-module(query_facility_h).

-export([init/2]).

init(Req0, Opts) ->
  {ok, Data, _} = cowboy_req:read_body(Req0),
  [Item|_] = jsx:decode(Data),
  Result = jsx:encode(query_facility_server:query_facility(Item)),
  Req = cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/json">>
     }, Result, Req0),
  {ok, Req, Opts}.