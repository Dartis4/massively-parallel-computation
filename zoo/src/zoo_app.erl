%%%-------------------------------------------------------------------
%% @doc zoo public API
%% @end
%%%-------------------------------------------------------------------

-module(zoo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    zoo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
