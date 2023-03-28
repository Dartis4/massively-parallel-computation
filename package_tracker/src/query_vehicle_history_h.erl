-module(query_vehicle_history_h).

-export([init/2]).

init(Req0, Opts) ->
  io:fwrite("Query Vehicle~n"),
  {ok, Data, _} = cowboy_req:read_body(Req0),
  Dict = jsx:decode(Data),
  case query_vehicle_history_server:query_vehicle_history(Dict) of
    {error, _} ->
      Req = cowboy_req:reply(200, #{
          <<"content-type">> => <<"text/json">>
         }, jsx:encode(none), Req0),
      {ok, Req, Opts};
    Query ->
      Req = cowboy_req:reply(200, #{
          <<"content-type">> => <<"text/json">>
         }, jsx:encode(Query), Req0),
      {ok, Req, Opts}
  end.
