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
-export([start_to_raised_above_Comp/1, raised_above_Comp_to_claw_Open/1, claw_Open_to_lowered_above_Comp/1,
         lowered_above_Comp_to_claw_Closed/1, claw_Closed_to_raised_above_Comp/1, above_Comp_to_above_Box/1,
         raised_above_Box_to_lowered/1, lowered_above_Box_to_claw_Open/1, claw_Open_to_raised_above_Box/1,
         above_Box_to_above_Comp/1]).


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
    gen_statem:start({local,Statem_name},?MODULE,Initial_state,[]).

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
init(Initial_state) ->
    %% Set the initial state to be the list of available Worker_ids
    %% and types.
    {ok,start,Initial_state}.
%% @private
callback_mode() -> handle_event_function.

%%% API functions

% Flow of states
% start -> raised_above_Comp -> 
%   claw_Open_raised_above_Comp -> claw_Open_lowered_above_Comp -> claw_Closed_lowered_above_Comp -> claw_Closed_raised_above_Comp -> 
%     claw_Closed_raised_above_Box -> claw_Closed_lowered_above_Box -> claw_Open_lowered_above_Box -> claw_Open_raised_above_Box -> 
%       claw_Open_raised_above_Comp

start_to_raised_above_Comp(Statem_name) ->
  gen_statem:call(Statem_name, to_raised_above_comp_initial).

raised_above_Comp_to_claw_Open(Statem_name) ->
  gen_statem:call(Statem_name, to_open_initial).

claw_Open_to_lowered_above_Comp(Statem_name) ->
  gen_statem:call(Statem_name, to_lowered_above_comp).

lowered_above_Comp_to_claw_Closed(Statem_name) ->
  gen_statem:call(Statem_name, to_closed).

claw_Closed_to_raised_above_Comp(Statem_name) ->
  gen_statem:call(Statem_name, to_raised_above_comp).

above_Comp_to_above_Box(Statem_name) ->
  gen_statem:call(Statem_name, to_above_box).

raised_above_Box_to_lowered(Statem_name) ->
  gen_statem:call(Statem_name, to_lowered_above_box).

lowered_above_Box_to_claw_Open(Statem_name) ->
  gen_statem:call(Statem_name, to_open).

claw_Open_to_raised_above_Box(Statem_name) ->
  gen_statem:call(Statem_name, to_raised_above_box).

above_Box_to_above_Comp(Statem_name) ->
  gen_statem:call(Statem_name, to_above_comp).

%%
%% Used to select which registered worker is to be used next in
%% a round robin fashion.
%% @private
-spec handle_event({call, From::pid()}, EventContent::term(), State::term(), Data::term()) ->
        {next_state,NewState::term(),Data::term(),[{reply,From::pid(),Data::term()}]}.
handle_event({call,From}, to_raised_above_comp_initial, start, Data) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state,raised_above_comp,Data,[{reply,From,raised_above_comp}]};

handle_event({call,From}, to_open_initial, raised_above_comp, Data) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state,claw_Open_raised_above_Comp,Data,[{reply,From,claw_Open_raised_above_Comp}]};

handle_event({call,From}, to_lowered_above_comp, claw_Open_raised_above_Comp, Data) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state,claw_Open_lowered_above_Comp,Data,[{reply,From,claw_Open_lowered_above_Comp}]};

handle_event({call,From}, to_closed, claw_Open_lowered_above_Comp, Data) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state,claw_Closed_lowered_above_Comp,Data,[{reply,From,claw_Closed_lowered_above_Comp}]};

handle_event({call,From}, to_raised_above_comp, claw_Closed_lowered_above_Comp, Data) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state,claw_Closed_raised_above_Comp,Data,[{reply,From,claw_Closed_raised_above_Comp}]};

handle_event({call,From}, to_above_box, claw_Closed_raised_above_Comp, Data) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state,claw_Closed_raised_above_Box,Data,[{reply,From,claw_Closed_raised_above_Box}]};

handle_event({call,From}, to_lowered_above_box, claw_Closed_raised_above_Box, Data) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state,claw_Closed_lowered_above_Box,Data,[{reply,From,claw_Closed_lowered_above_Box}]};

handle_event({call,From}, to_open, claw_Closed_lowered_above_Box, Data) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state,claw_Open_lowered_above_Box,Data,[{reply,From,claw_Open_lowered_above_Box}]};

handle_event({call,From}, to_raised_above_box, claw_Open_lowered_above_Box, Data) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state,claw_Open_raised_above_Box,Data,[{reply,From,claw_Open_raised_above_Box}]};

handle_event({call,From}, to_above_comp, claw_Open_raised_above_Box, Data) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state,claw_Open_raised_above_Comp,Data,[{reply,From,claw_Open_raised_above_Comp}]}.


%% This code is included in the compiled code only if
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
%%
%% Unit tests go here.
%%
start_test() ->
  {setup,
  fun() -> donothing end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({ok, _}, gen_statem:start_link({local,?SERVER},?MODULE,[],[]))]}.

start_to_above_comp_test() ->
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, start, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({next_state,raised_above_comp,[],[{reply,nil,raised_above_comp}]}, handle_event({call, nil}, to_raised_above_comp_initial, start, [])),
   ?assertMatch(raised_above_comp, gen_statem:call(?SERVER, to_raised_above_comp_initial))]}.

above_comp_to_open_above_comp_test() ->
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, above_comp, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({next_state,claw_Open_raised_above_Comp,[],[{reply,nil,claw_Open_raised_above_Comp}]}, handle_event({call, nil}, to_open_initial, raised_above_comp, []))]}.

open_above_comp_to_lowered_above_comp_test() ->
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, open_above_comp, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({next_state,claw_Open_lowered_above_Comp,[],[{reply,nil,claw_Open_lowered_above_Comp}]}, handle_event({call, nil}, to_lowered_above_comp, claw_Open_raised_above_Comp, []))]}.

lowered_above_comp_to_claw_closed_test() ->
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, lowered_above_comp, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({next_state,claw_Closed_lowered_above_Comp,[],[{reply,nil,claw_Closed_lowered_above_Comp}]}, handle_event({call, nil}, to_closed, claw_Open_lowered_above_Comp, []))]}.

claw_closed_to_raised_above_comp_test() ->
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, claw_closed, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({next_state,claw_Closed_raised_above_Comp,[],[{reply,nil,claw_Closed_raised_above_Comp}]}, handle_event({call, nil}, to_raised_above_comp, claw_Closed_lowered_above_Comp, []))]}.

raised_above_comp_to_above_box_test() ->
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, rasied_above_comp, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({next_state,claw_Closed_raised_above_Box,[],[{reply,nil,claw_Closed_raised_above_Box}]}, handle_event({call, nil}, to_above_box, claw_Closed_raised_above_Comp, []))]}.

above_box_to_lowered_above_box_test() ->
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, above_box, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({next_state,claw_Closed_lowered_above_Box,[],[{reply,nil,claw_Closed_lowered_above_Box}]}, handle_event({call, nil}, to_lowered_above_box, claw_Closed_raised_above_Box, []))]}.

lowered_above_box_to_claw_open_test() ->
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, lowered_above_box, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({next_state,claw_Open_lowered_above_Box,[],[{reply,nil,claw_Open_lowered_above_Box}]}, handle_event({call, nil}, to_open, claw_Closed_lowered_above_Box, []))]}.

claw_open_to_raised_above_box_test() ->
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, claw_open, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({next_state,claw_Open_raised_above_Box,[],[{reply,nil,claw_Open_raised_above_Box}]}, handle_event({call, nil}, to_raised_above_box, claw_Open_lowered_above_Box, []))]}.

raised_above_box_to_above_comp_test() ->
  {setup,
  fun() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, above_box, []) end,
  fun() -> gen_statem:stop(?SERVER) end,
  [?assertMatch({next_state,claw_Open_raised_above_Comp,[],[{reply,nil,claw_Open_raised_above_Comp}]}, handle_event({call, nil}, to_above_comp, claw_Open_raised_above_Box, []))]}.

-endif.
