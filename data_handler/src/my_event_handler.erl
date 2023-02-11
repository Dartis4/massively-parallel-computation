-module(my_event_handler).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-record(my_event_handler_state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================
start_link() ->
  gen_event:start_link({local, ?SERVER}).

add_handler() ->
  gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================
init([]) ->
  {ok, #my_event_handler_state{}}.

handle_event({success, Message},  State = #my_event_handler_state{}) ->
  io:format("Success, with message: ~s ~n", [Message]),
  {ok, State}.

handle_call(_Request, State = #my_event_handler_state{}) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State = #my_event_handler_state{}) ->
  {ok, State}.

terminate(_Arg, _State = #my_event_handler_state{}) ->
  ok.

code_change(_OldVsn, State = #my_event_handler_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
