%%%-------------------------------------------------------------------
%% @doc robot_arm_sm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(robot_arm_sm_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
% generate_spec(Module,Name,Type)->
%   #{id => Name,
%     start => {Module, start, [local, Name, [math1]]},
%     restart => permanent,
%     shutdown => 2000,
%     type => Type,
%     modules => [Module]}.

% generate_spec(Module,Name,Type,Data)->
%   #{id => Name,
%     start => {Module, start, [local, Name, Data]},
%     restart => permanent,
%     shutdown => 2000,
%     type => Type,
%     modules => [Module]}.
