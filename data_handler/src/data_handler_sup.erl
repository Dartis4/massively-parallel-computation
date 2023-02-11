%%%-------------------------------------------------------------------
%% @doc data_handler top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(data_handler_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  Res = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
  ok = gen_event:add_handler(my_event, my_event_handler, []),
  Res.

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
  Manager_event = #{id => my_event,
    start => {gen_event, start_link, [{local, my_event}]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [dynamic]},

  SupFlags = #{strategy => one_for_all,
    intensity => 0,
    period => 1},

  ChildSpecs = [Manager_event],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
