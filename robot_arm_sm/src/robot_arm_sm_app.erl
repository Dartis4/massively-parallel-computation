%%%-------------------------------------------------------------------
%% @doc robot_arm_sm public API
%% @end
%%%-------------------------------------------------------------------

-module(robot_arm_sm_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    robot_arm_sm_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
