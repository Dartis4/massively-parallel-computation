%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
-module(store_package_info_server).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0,start/3,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([store_package/2]).


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
-spec start(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name,Args) ->
  gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).


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
  riakc_pb_socket:start_link("database.dartis.dev", 8087).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::pid(), State::term()) ->
  {reply, term(), term()} |
  {reply, term(), term(), integer()} |
  {noreply, term()} |
  {noreply, term(), integer()} |
  {stop, term(), term(), integer()} | 
  {stop, term(), term()}.

handle_call(stop, _From, _State) ->
  {stop,normal,
   replace_stopped,
   down}; %% setting the server's internal state to down
handle_call(_Request, _From, State) ->
  {reply,replace_started,State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::term()) -> {noreply, term()} |
                                                 {noreply, term(), integer()} |
                                                 {stop, term(), term()}.
handle_cast({store_package, Key, Value}, Riak_Pid) ->
  case Key =:= <<"">> of 
    true ->
      {noreply, Riak_Pid};
    _ ->
      Package = riakc_obj:new(<<"package">>, Key, Value),
      _Status = riakc_pb_socket:put(Riak_Pid, Package),
      {noreply, Riak_Pid}
  end;
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info::term(), State::term()) -> {noreply, term()} |
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
-spec terminate(Reason::term(), term()) -> term().
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

store_package(Package_uuid, Holders) ->
  gen_server:cast(?SERVER, {store_package, Package_uuid, Holders}).


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
store_package_mock_riak_test_() ->
  {setup,
   fun() -> 
       meck:new(riakc_obj),
       meck:new(riakc_pb_socket),
       meck:expect(riakc_obj, new, fun(_Bucket, _Key, _Data) -> request end),
       meck:expect(riakc_pb_socket, put, fun(_Riak_pid, _Request) -> status end)
   end,
   fun(_) -> 
       meck:unload(riakc_obj),
       meck:unload(riakc_pb_socket)
   end,
   [
    ?_assertMatch({noreply, riak_pid}, ?MODULE:handle_cast({store_package, <<"package_uuid">>, [<<"holder_uuid">>]}, riak_pid)),
    ?_assertMatch({noreply, riak_pid}, ?MODULE:handle_cast({store_package, <<"package_uuid">>, []}, riak_pid)),
    ?_assertMatch({noreply, riak_pid}, ?MODULE:handle_cast({store_package, <<"">>, []}, riak_pid))
   ]
  }.
store_in_riak_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end, 
   [
    ?_assertMatch(ok, store_package(<<"P123">>, []))
   ]
  }.
-endif.

