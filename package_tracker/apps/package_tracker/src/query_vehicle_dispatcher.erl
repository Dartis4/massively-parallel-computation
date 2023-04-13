-module(query_vehicle_dispatcher).
-behaviour(gen_statem).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/2,start_link/1,start_link/2,stop/1]).

%% Supervisor Callbacks
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
%% State Callbacks
-export([handle_event/4]).
-export([iterate_worker/1]).


%%%===================================================================
%%% Public API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),term()) -> {ok, atom()}.


start(Statem_name,Workers) ->
    gen_statem:start({local,Statem_name},?MODULE,Workers,[]).

%%--------------------------------------------------------------------
%% @doc
%% 
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(term()) -> {ok, atom()}.


start_link(Workers) ->
    gen_statem:start_link({local,?MODULE},?MODULE,Workers,[]).

start_link(Name, Workers) ->
    gen_statem:start_link({local,Name},?MODULE,Workers,[]).

%%--------------------------------------------------------------------
%% @doc
%% This function gracefully shuts down the balancer.
%%
%% The parameter of stop is an atom that
%% is a registered name of a round robin balancer.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(atom()) -> ok.
stop(Statem_name) ->
    gen_statem:stop(Statem_name).

%% Mandatory callback functions
%% @private
terminate(_Reason, _State, _Data) ->
    void.
%% @private
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
%% @private
init(Worker_ids) ->
    %% Set the initial state to be the list of available Worker_ids
    %% and types.
    {ok,ready,Worker_ids}.
%% @private
callback_mode() -> handle_event_function.

%%% API functions
iterate_worker(Bal_id) ->
  gen_statem:call(Bal_id, blib).

%%
%% Used to select which registered worker is to be used next in 
%% a round robin fashion.
%% @private
-spec handle_event({call, From::pid()}, EventContent::term(), State::term(), Data::term()) ->
        {next_state,NewState::term(),Data::term(),[{reply,From::pid(),Data::term()}]}.


handle_event({call,From},blib,ready,[Registered_name|T]) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state,ready,T++[Registered_name],[{reply,From,Registered_name}]};

% handle_event({call, From},Command,_State, Worker_ids) ->
%   {next_state,fail, Worker_ids,[{reply,From,{error,?MODULE,Command}}]};

handle_event({call, From},Command, _State, Worker_ids) ->
  {next_state,fail, Worker_ids,[{reply,From,{error,?MODULE,From,Command}}]}.

%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
start_test_() ->
  {setup,
  fun() -> donothing end,
  fun(_) -> gen_statem:stop(start_tester) end,
  [?_assertMatch({ok, _}, gen_statem:start_link({local,start_tester},?MODULE,[],[]))]}.

start_with_ids_test_() ->
  {setup,
  fun() -> donothing end,
  fun(_) -> gen_statem:stop(start_ids_tester) end,
  [?_assertMatch({ok, _}, start_link(start_ids_tester,[worker1,worker2,worker3]))]}.

get_next_worker_test_() ->
  {setup,
  fun() -> donothing end,
  fun(_) -> stop(next_worker) end,
  [?_assertMatch({ok, _}, start_link(next_worker,[worker1,worker2,worker3])),
   ?_assertEqual(worker1, iterate_worker(next_worker))]}.
-endif.
