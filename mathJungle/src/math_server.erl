-module(math_server).
-behaviour(gen_server).

-export([start/0, start/2, stop/0, stop/1, add/3, subtract/3, multiply/3, divide/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% APIs
%%%===================================================================
start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(Registration_type, Registration_name) ->
  gen_server:start_link({Registration_type, Registration_name}, ?MODULE, [], []).

stop() ->
  gen_server:stop(?MODULE).

stop(Registration_name) ->
  gen_server:stop(Registration_name).

%%Math External APIs
add(Registration_name, First_Number, Second_Number) ->
  gen_server:call(Registration_name, {add, First_Number, Second_Number}).

subtract(Registration_name, First_Number, Second_Number) ->
  gen_server:call(Registration_name, {subtract, First_Number, Second_Number}).

multiply(Registration_name, First_Number, Second_Number) ->
  gen_server:call(Registration_name, {multiply, First_Number, Second_Number}).

divide(Registration_name, First_Number, Second_Number) ->
  gen_server:call(Registration_name, {divide, First_Number, Second_Number}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
init(State) ->
  {ok, State}.

%%Add two Numbers
handle_call({add, First_Number, Second_Number}, _From, _State) ->
  {reply, {ok, First_Number + Second_Number}, _State};

%%Subtract two Numbers
handle_call({subtract, First_Number, Second_Number}, _From, _State) ->
  {reply, {ok, First_Number - Second_Number}, _State};

%%Multiple two Numbers
handle_call({multiply, First_Number, Second_Number}, _From, _State) ->
  {reply, {ok, First_Number * Second_Number}, _State};

%%Divide two Numbers
handle_call({divide, First_Number, Second_Number}, _From, _State) ->
  {reply, {ok, First_Number / Second_Number}, _State}.



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
      gen_server:stop(test_start)
    end,
    [?assertMatch({ok, _}, start()),
      ?assertMatch({ok, _}, start(local, test_start))]}.

add_handle_call_test() ->
  {setup,
    fun() -> gen_server:start({local, add_}, ?MODULE, [], []) end,
    fun() -> gen_server:stop(add_) end,
    [?assertEqual({reply, {ok, 89}, []}, handle_call({add, 44, 45}, nil, []))]}.

subtract_handle_call_test() ->
  {setup,
    fun() -> gen_server:start({local, subtract_}, ?MODULE, [], []) end,
    fun() -> gen_server:stop(subtract_) end,
    [?assertEqual({reply, {ok, 6}, []}, handle_call({subtract, 78, 72}, nil, []))]}.

multiply_handle_call_test() ->
  {setup,
    fun() -> gen_server:start({local, multiply_}, ?MODULE, [], []) end,
    fun() -> gen_server:stop(multiply_) end,
    [?assertEqual({reply, {ok, 12}, []}, handle_call({multiply, 3, 4}, nil, []))]}.


divide_handle_call_test() ->
  {setup,
    fun() -> gen_server:start({local, divide_}, ?MODULE, [], []) end,
    fun() -> gen_server:stop(divide_) end,
    [?assertEqual({reply, {ok, 4.0}, []}, handle_call({divide, 88, 22}, nil, []))]}.

add_test() ->
  start(local, adder),
  ?assertEqual({ok, 165}, add(adder, 145, 20)),
  stop(adder).

subtract_test() ->
  start(local, minus),
  ?assertEqual({ok, -1}, subtract(minus, 1, 2)),
  stop(minus).

multiply_test() ->
  start(local, multiplier),
  ?assertEqual({ok, -672}, multiply(multiplier, -21, 32)),
  stop(multiplier).

divide_test() ->
  start(local, divider),
  ?assertEqual({ok, -5.0}, divide(divider, -20, 4)),
  stop(divider).

- endif.

