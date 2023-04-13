%%%-------------------------------------------------------------------
%% @doc data_handler public API
%% @end
%%%-------------------------------------------------------------------

-module(data_handler_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    data_handler_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
