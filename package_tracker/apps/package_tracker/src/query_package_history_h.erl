-module(query_package_history_h).

-export([init/2]).

init(Req0, Opts) ->
  io:fwrite("Query Package~n"),
  {ok, Data, _} = cowboy_req:read_body(Req0),
  Dict = jsx:decode(Data),
  Worker = query_package_dispatcher:iterate_worker(query_package_dispatcher),
  case query_package_history_server:query_package_history(Worker, Dict) of
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
