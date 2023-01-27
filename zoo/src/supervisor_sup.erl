-module(supervisor_sup).
-behaviour(supervisor).

-export([start/0, start/1, start/3, init/1]).

-define(SERVER, ?MODULE).

start() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start(Start_info)-> supervisor:start_link({local, ?MODULE}, ?MODULE, Start_info).

start(Supervisor_name, Registration_type, Start_info)->
    supervisor:start_link({Registration_type, Supervisor_name}, ?MODULE, Start_info).

init(Start_info) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 10,
                 period => 1},
    ChildSpecs = [generate_spec(zoo_sup, supervisor, Start_info),
                  generate_spec(dynamic_zoo_sup, supervisor, Start_info)],
    {ok, {SupFlags, ChildSpecs}}.

generate_spec(Module, Type, Start_info) ->
    #{id => Module,
      start => {Module, start, Start_info},
      restart => permanent,
      shutdown => 2000,
      type => Type,
      modules => [Module]}.

