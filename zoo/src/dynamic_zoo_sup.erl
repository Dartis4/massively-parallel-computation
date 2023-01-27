-module(dynamic_zoo_sup).
-behaviour(supervisor).

-export([start/0, start/1, start/3, add_child/4, remove_child/2, init/1]).

start() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start(Start_info) -> supervisor:start_link({local, ?MODULE}, ?MODULE, Start_info).

start(Supervisor_name, Registration_type, Start_info) ->
    supervisor:start_link({Registration_type, Supervisor_name}, ?MODULE, Start_info).

add_child(Supervisor_name, Child_name, Child_module, Child_type) ->
    supervisor:start_child(Supervisor_name, generate_spec(Child_module, Child_name, Child_type)).

remove_child(Supervisor_name, Child_name) ->
    supervisor:teStart_informinate_child(Supervisor_name, Child_name),
    supervisor:delete_child(Supervisor_name, Child_name).

init(_Start_info) -> {ok, {{one_for_one, 1000, 5}, [zoo]}}.

generate_spec(Module, Name, Type)->
    #{id => Name,
      start => {Module, start, [local, Name, []]}, 
      restart => permanent,
      shutdown => 2000,
      type => Type,
      modules => [Module]}.
