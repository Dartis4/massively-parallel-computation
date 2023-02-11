%%%-------------------------------------------------------------------
%%% @author Lee Barney  <barney.cit@gmail.com>
%%% @copyright © 2022, Lee S. Barney
%%% @reference Licensed under the 
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% This is a round robin balancer. Given a set of module-id pairs, this balancer
%%% will distribute work in a  
%%% <a href="https://www.techtarget.com/whatis/definition/round-robin">
%%% round-robin</a> fashion.
%%%
%%% To use this round robin balancer, the balanced worker item must have a
%%% locally or globally registered name. The registered name is used 
%%% to add the item to a balancer.
%%%
%%%
%%%
%%% Be aware that a worker item can, via its ID, be added to more than 
%%% one rr_balancer. This is by design, not by accident. 
%%% @end

%%% Created : 24 June 2022 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------
-module(dispatcher).
-behaviour(gen_statem).

-define(SERVER, ?MODULE).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/2,start_link/2,stop/1]).

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
-spec start_link(atom(),term()) -> {ok, atom()}.
start_link(Statem_name,Workers) ->
    gen_statem:start_link({local,Statem_name},?MODULE,Workers,[]).


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

handle_event({call, From},Command,_State,_Worker_ids) ->
  {next_state,fail,_Worker_ids,[{reply,From,{error,?MODULE,Command}}]}.

handle_event({call, From},Command,_State,_Worker_ids) ->
  {next_state,fail,_Worker_ids,[{reply,From,{error,?MODULE,From,Command}}]}.

%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
start_test() ->
  {setup,
  fun() -> donothing end,
  fun() -> gen_statem:stop(start_tester) end,
  [?assertMatch({ok, _}, gen_statem:start_link({local,start_tester},?MODULE,[],[]))]}.

start_with_ids_test() ->
  {setup,
  fun() -> donothing end,
  fun() -> gen_statem:stop(start_ids_tester) end,
  [?assertMatch({ok, _}, start_link(start_ids_tester,[worker1,worker2,worker3]))]}.

get_next_worker_test() ->
  {setup,
  fun() -> donothing end,
  fun() -> stop(next_worker) end,
  [?assertMatch({ok, _}, start_link(next_worker,[worker1,worker2,worker3])),
   ?assertEqual(worker1, iterate_worker(next_worker))]}.

-endif.
