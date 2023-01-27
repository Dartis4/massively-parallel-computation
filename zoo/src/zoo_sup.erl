%%%-------------------------------------------------------------------
%% @doc zoo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(zoo_sup).

-behaviour(supervisor).

-export([start/0, start/1, start/3]).

-export([init/1]).

-define(SERVER, ?MODULE).

start() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start(Start_info) -> supervisor:start_link({local, ?SERVER}, ?MODULE, Start_info).

start(Supervisor_name, Registration_type, Start_info) ->
  supervisor:start_link({Registration_type, Supervisor_name}, ?MODULE, Start_info).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init(Start_info) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1000,
                 period => 5},
    ChildSpecs = [generate_spec(zoo, worker, Start_info)],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
generate_spec(Module, Type, Start_info) ->
  #{id => Module,
   start => {Module, start, Start_info},
   restart => permanent,
   shutdown => 2000,
   type => Type,
   modules => [Module]}.
