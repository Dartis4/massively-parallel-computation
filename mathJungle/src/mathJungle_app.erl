%%%-------------------------------------------------------------------
%% @doc mathJungle public API
%% @end
%%%-------------------------------------------------------------------

-module(mathJungle_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mathJungle_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
