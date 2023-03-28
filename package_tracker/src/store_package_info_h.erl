-module(store_package_info_h).

-export([init/2]).

init(Req0, Opts) ->
  io:fwrite("Store Package~n"),
  {ok, Data, _} = cowboy_req:read_body(Req0),
  Dict = jsx:decode(Data),
  Result = jsx:encode(store_package_info_server:store_package_info(Dict)),
  Req = cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/json">>
     }, Result, Req0),
  {ok, Req, Opts}.
