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
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    ok = gen_event:add_handler(?SERVER, sms_event, []),
    {ok, Pid}.

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
    Children = [{my_event, {gen_event, start_link, [{local, my_event}]},permanent, 5000, worker, [dynamic]}],
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    {ok, {SupFlags, Children}}.

%% internal functions
