%%%-------------------------------------------------------------------
%% @doc package_tracker public API
%% @end
%%%-------------------------------------------------------------------

-module(package_tracker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  io:fwrite("App Started~n"),
  Dispatch = cowboy_router:compile([ 
    {'_', [ 
      {"/", toppage_h, []},
      {"/store_package_info", store_package_info_h, []},
      {"/query_package_history", query_package_history_h, []},
      {"/store_vehicle_info", store_vehicle_info_h, []},
      {"/query_vehicle_history", query_vehicle_history_h, []},
      {"/store_facility_info", store_facility_info_h, []},
      {"/query_facility", query_facility_h, []}
    ]}
  ]),

  PrivDir = code:priv_dir(package_tracker),
  {ok, _} = cowboy:start_tls(https_listener, [
    {port, 443},
    {certfile, PrivDir ++ "/ssl/fullchain.pem"},
    {keyfile, PrivDir ++ "/ssl/privkey.pem"}
  ], #{env => #{dispatch => Dispatch}}),
  package_tracker_sup:start_link().

stop(_State) ->
  ok.

%% internal functions
