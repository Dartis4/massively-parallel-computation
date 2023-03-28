-module(store_vehicle_info_h).

-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, Opts) ->
  io:fwrite("Store Vehicle~n"),
  {ok, Data, _} = cowboy_req:read_body(Req0),
  Dict = jsx:decode(Data),
  Result = jsx:encode(store_vehicle_info_server:store_vehicle_info(Dict)),
  Req = cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/json">>
     }, Result, Req0),
  {ok, Req, Opts}.
