%%%-------------------------------------------------------------------
%% @doc mathJungle top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mathJungle_sup).

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
  Dynamic_MathJungle_Sup = #{id => dynamic_mathJungle_sup,
    start => {dynamic_mathJungle_sup, start, []},
    restart => permanent,
    shutdown => 2000,
    type => supervisor,
    modules => [dynamic_mathJungle_sup]},

  SupFlags = #{strategy => one_for_all,
    intensity => 0,
    period => 1},
  ChildSpecs = [Dynamic_MathJungle_Sup],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
