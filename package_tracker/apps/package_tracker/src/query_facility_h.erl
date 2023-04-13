-module(query_facility_h).

-export([init/2]).

init(Req0, Opts) ->
  io:fwrite("Query Facility~n"),
  {ok, Data, _} = cowboy_req:read_body(Req0),
  Dict = jsx:decode(Data),
  Worker = query_facility_dispatcher:iterate_worker(query_facility_dispatcher),
  case query_facility_server:query_facility(Worker, Dict) of
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
