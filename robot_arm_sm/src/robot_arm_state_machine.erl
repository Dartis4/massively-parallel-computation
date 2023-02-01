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
-module(robot_arm_state_machine).
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
start(Statem_name,Initial_state) ->
    gen_statem:start({local,Statem_name}, ?MODULE, Initial_state, []).

%%--------------------------------------------------------------------
%% @doc
%% 
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(),term()) -> {ok, atom()}.
start_link(Statem_name,Initial_state) ->
    gen_statem:start_link({local,Statem_name},?MODULE,Initial_state,[]).


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

start_to_above_comp() -> pass.

above_comp_to_open_above_comp() -> pass.

open_above_comp_to_lowered_above_comp() -> pass.

lowered_above_comp_to_claw_closed() -> pass.

claw_closed_to_raised_above_comp() -> pass.

raised_above_comp_to_above_box() -> pass.

above_box_to_lowered_above_box() -> pass.

lowered_above_box_to_claw_open() -> pass.

claw_open_to_raised_above_box() -> pass.

raised_above_box_to_above_comp() -> pass.

%%
%% Used to select which registered worker is to be used next in 
%% a round robin fashion.
%% @private
handle_event({call,From}, next, ready,{Statem_name,State_data}) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, ready,{Statem_name,State_data},[{reply,From,Statem_name}]}.

%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
start_to_above_comp_test() -> 
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, start, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({ok, _}, handle_event({call, nil}, above_comp, start, {?SERVER, nil}))]}.

above_comp_to_open_above_comp_test() -> 
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, above_comp, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({ok, _}, handle_event({call, nil}, open_above_comp, above_comp, {?SERVER, nil}))]}.

open_above_comp_to_lowered_above_comp_test() -> 
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, open_above_comp, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({ok, _}, handle_event({call, nil}, lowered_above_comp, open_above_comp, {?SERVER, nil}))]}.

lowered_above_comp_to_claw_closed_test() -> 
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, lowered_above_comp, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({ok, _}, handle_event({call, nil}, claw_closed, lowered_above_comp, {?SERVER, nil}))]}.

claw_closed_to_raised_above_comp_test() -> 
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, claw_closed, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({ok, _}, handle_event({call, nil}, raised_above_comp, claw_closed, {?SERVER, nil}))]}.

raised_above_comp_to_above_box_test() -> 
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, rasied_above_comp, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({ok, _}, handle_event({call, nil}, above_box, raised_above_comp, {?SERVER, nil}))]}.

above_box_to_lowered_above_box_test() -> 
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, above_box, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({ok, _}, handle_event({call, nil}, lowered_above_box, above_box, {?SERVER, nil}))]}.

lowered_above_box_to_claw_open_test() -> 
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, lowered_above_box, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({ok, _}, handle_event({call, nil}, claw_open, lowered_above_box, {?SERVER, nil}))]}.

claw_open_to_raised_above_box_test() -> 
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, claw_open, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({ok, _}, handle_event({call, nil}, above_box, claw_open, {?SERVER, nil}))]}.

raised_above_box_to_above_comp_test() -> 
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, above_box, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({ok, _}, handle_event({call, nil}, above_comp, above_box, {?SERVER, nil}))]}.

-endif.
