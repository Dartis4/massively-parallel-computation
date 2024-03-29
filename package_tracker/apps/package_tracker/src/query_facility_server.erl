%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
-module(query_facility_server).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0, start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-export([query_facility/1,query_facility/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server assuming there is only one server started for 
%% this module. The server is registered locally with the registered
%% name being the name of the module.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%% Args is a list containing any data to be passed to the gen_server's
%% init function.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).

%% Any other API functions go here.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init([]) ->
  case riakc_pb_socket:start_link("database.dartis.dev", 8087) of
    {ok,Riak_pid} -> 
      {ok,Riak_pid};
    _ ->
      {stop,link_failure}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: pid(), State :: term()) ->
  {reply, term(), term()} |
  {reply, term(), term(), integer()} |
  {noreply, term()} |
  {noreply, term(), integer()} |
  {stop, term(), term(), integer()} |
  {stop, term(), term()}.

handle_call({get_facility, Facility_uuid}, _From, Riak_Pid) ->
  %{reply,<<bob,sue,alice>>,Riak_PID};
  case riakc_pb_socket:get(Riak_Pid, <<"facility">>, Facility_uuid) of
    {ok, Fetched} ->
      %reply with the value as a binary, not the key nor the bucket.
      {reply, binary_to_term(riakc_obj:get_value(Fetched)), Riak_Pid};
    Error ->
      {reply, Error, Riak_Pid}
  end;
handle_call(stop, _From, _State) ->
  {stop, normal,
    replace_stopped,
    down}. %% setting the server's internal state to down

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg :: term(), State :: term()) -> {noreply, term()} |
{noreply, term(), integer()} |
{stop, term(), term()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info :: term(), State :: term()) -> {noreply, term()} |
{noreply, term(), integer()} |
{stop, term(), term()}.
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), term()) -> term().
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

query_facility(Data) -> 
  gen_server:call(?SERVER, {get_facility, maps:get(<<"facility_uuid">>, Data)}).

query_facility(Name, Data) -> 
  gen_server:call(Name, {get_facility, maps:get(<<"facility_uuid">>, Data)}).


-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
server_start_test_()->
  {setup,
   fun()-> 
       meck:new(riakc_pb_socket), 
       meck:expect(riakc_pb_socket, start_link, fun(_Domain, _Port) -> {ok, riak_pid} end)
   end,
   fun(_)-> 
       gen_server:stop(?SERVER),
       meck:unload(riakc_pb_socket)
   end,
   [
    ?_assertMatch({ok, _}, start())
   ]
  }.
query_facility_riak_test_() ->
  {setup,
    fun() ->
      meck:new(riakc_obj),
      meck:new(riakc_pb_socket),
      meck:expect(riakc_obj, get_value, fun(_Key) -> term_to_binary("history") end),
      meck:expect(riakc_pb_socket, get, fun(_Riak_Pid, _Bucket, _Key) -> {ok, riakc_obj} end)
    end,
    fun(_) ->
      meck:unload(riakc_obj),
      meck:unload(riakc_pb_socket)
    end,
    [
      ?_assertMatch({reply, "history", riak_pid}, query_facility_server:handle_call({get_facility, <<"facility_uuid">>}, from, riak_pid)),
      ?_assertMatch({reply, "history", riak_pid}, query_facility_server:handle_call({get_facility, <<"">>}, from, riak_pid))
    ]
  }.
-endif.

