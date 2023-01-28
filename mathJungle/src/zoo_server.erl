-module(zoo_server).
-behaviour(gen_server).

-export([start/0, start/3, stop/0, stop/1, add/2, remove/2, list/1, remove_all/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% APIs
%%%===================================================================

start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(Registration_type, Registration_name, State) ->
  gen_server:start_link({Registration_type, Registration_name}, ?MODULE, State, []).

stop() ->
  gen_server:stop(?MODULE).

stop(Registration_name) ->
  gen_server:stop(Registration_name).

add(Registration_name, Animal) ->
  gen_server:call(Registration_name, {add, Animal}).

list(Registration_name) ->
  gen_server:call(Registration_name, list).

remove(Registration_name, Animal) ->
  gen_server:call(Registration_name, {remove, Animal}).

remove_all(Registration_name) ->
  gen_server:call(Registration_name, remove_all).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
init(State) ->
  {ok, State}.

handle_call({add, Animal}, _From, Animals) ->
  {reply, ok, [Animal] ++ Animals};
handle_call(list, _From, Animals) ->
  {reply, {ok, Animals}, Animals};
handle_call({remove, Animal}, _From, Animals) ->
  {reply, ok, lists:delete(Animal, Animals)};
handle_call(remove_all, _From, _Animals) ->
  {reply, ok, []}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%% --------------------------
%%% Unit Tests
%%% --------------------------
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
  {setup,
    fun() -> nil end,
    fun() ->
      gen_server:stop(?MODULE),
      gen_server:stop(testing_name)
    end,
    [?assertMatch({ok, _}, start()),
      ?assertMatch({ok, _}, start(local, testing_name, []))]}.

add_test() ->
  {setup,
    fun() -> gen_server:start({local, add_}, ?MODULE, [], []) end,
    fun() -> gen_server:stop(add_) end,
    [?assertEqual({reply, ok, [lion]}, handle_call({add, lion}, nil, [])),
      ?assertEqual({reply, ok, [lion, zebra, giraffe, hippo]},
        handle_call({add, lion}, nil, [zebra, giraffe, hippo]))]}.

list_test() ->
  {setup,
    fun() -> gen_server:start({local, list_}, ?MODULE, [], []) end,
    fun() -> gen_server:stop(list_) end,
    [?assertEqual({reply, {ok, []}, []}, handle_call(list, nil, [])),
      ?assertEqual({reply, {ok, [lion, zebra, giraffe, hippo]}, [lion, zebra, giraffe, hippo]},
        handle_call(list, nil, [lion, zebra, giraffe, hippo]))]}.

remove_test() ->
  {setup,
    fun() -> gen_server:start_link({local, remove_}, ?MODULE, [], []) end,
    fun() -> gen_server:stop(remove_) end,
    [?assertEqual({reply, ok, []}, handle_call({remove, lion}, nil, [lion])),
      ?assertEqual({reply, ok, [lion, zebra, giraffe]},
        handle_call({remove, hippo}, nil, [lion, zebra, giraffe, hippo]))]}.

remove_all_test() ->
  {setup,
    fun() -> gen_server:start({local, remove_all_}, ?MODULE, [], []) end,
    fun() -> gen_server:stop(remove_all_) end,
    [?assertEqual({reply, ok, []}, handle_call(remove_all, nil, [])),
      ?assertEqual({reply, ok, []},
        handle_call(remove_all, nil, [lion, zebra, giraffe, hippo]))]}.

-endif.

