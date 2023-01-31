%%%-------------------------------------------------------------------
%%% @author gabrielikpaetuk
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(dynamic_mathJungle_sup).

-behaviour(supervisor).

-export([start/0, start/1, add_child/4, remove_child/2]).
-export([init/1]).


%%%===================================================================
%%% APIs
%%%===================================================================
start() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start(Start_info) -> supervisor:start_link({local, ?MODULE}, ?MODULE, Start_info).

add_child(Supervisor_name, Child_name, Child_module, Child_type) ->
  supervisor:start_child(Supervisor_name, generate_spec(Child_module, Child_name, Child_type)).

remove_child(Supervisor_name, Child_name) ->
  supervisor:terminate_child(Supervisor_name, Child_name),
  supervisor:delete_child(Supervisor_name, Child_name).


%%%===================================================================
%%% supervisor implementation
%%%===================================================================
init([]) ->
  %% TODO:  Use generateSpec()
  Zoo_Server = generate_spec(zoo_server, zoo, worker),
  Math_Server = generate_spec(math_server, math, worker),

  {ok, {#{strategy => one_for_one,
    intensity => 1000,
    period => 5},
    [Zoo_Server, Math_Server]}
  }.


%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_spec(Module,Name,Type)->
  #{id => Name,
    start => {Module, start, [local, Name, []]},
    restart => permanent,
    shutdown => 2000,
    type => Type,
    modules => [Module]}.