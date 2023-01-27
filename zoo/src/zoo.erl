-module(zoo).

-behaviour(gen_server).

-export([start/0, start/3, stop/0, add/2, list/1, remove/2, remove_all/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% --------------------------
%%% Client Functions
%%% --------------------------

-spec start() -> {ok, pid()} | {error, Reason :: term()}.
% @doc
% Spawns the zoo {@type gen_server}. Only one zoo {@type gen_server} can be created with this function.
%
% @returns a 2-tuple consisting of _ok_ followed by the process ID of the running zoo {@type gen_server}
%
% @end
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start(atom(), atom(), List :: atom()) -> {ok, pid()} | {error, Reason :: term()}.
% @doc
% Spawns a zoo _{@type gen_server}_ using the provided server type
% _Registration_type_ and the given server name _Registration_name_ and initializes the
% server with state _Args_.
%
% @param Registration_type the atoms _local_ or _global_.
%
% @param Registration_name the name used to register the server; must be an {@type atom}.
%
% @param State the initial state of the server. This will be a {@type list} of {@type atoms}.
%
% @returns a 2-tuple consisting of _ok_ followed by the process ID of the running zoo {@type gen_server} with the provided registered name.
%
% @end
start(Registration_type, Registration_name, State) ->
  gen_server:start_link({Registration_type, Registration_name}, ?MODULE, State, []).

-spec stop() -> ok.
% @doc
% Terminates the zoo {@type gen_server}.
%
% @returns the {@type atom} _ok_.
%
% @end
stop() ->
  gen_server:call(?MODULE, stop).

-spec add(atom(), atom()) -> ok.
% @doc
% Adds the provided animal name to the list of animals.
%
% @param Registration_name the name of the server.
%
% @param Animal the name of a zoo animal as an {@type atom}.
%
% @returns the {@type atom} _ok_.
%
% @end
add(Registration_name, Animal) ->
  gen_server:call(Registration_name, {add, Animal}).

-spec list(atom()) -> {ok, List :: atom()}.
% @doc
% Outputs the current list of animals.
%
% @param Registration_name the name of the server.
%
% @returns the {@type atom} _ok_ and the list of animals.
%
% @end
list(Registration_name) ->
  gen_server:call(Registration_name, list).

-spec remove(atom(), atom()) -> ok.
% @doc
% Removes the given animal name from the list of animals.
%
% @param Registration_name the name of the server.
%
% @param Animal the name of a zoo animal as an {@type atom}.
%
% @returns the {@type atom} _ok_.
%
% @end
remove(Registration_name, Animal) ->
  gen_server:call(Registration_name, {remove, Animal}).

-spec remove_all(atom()) -> ok.
% @doc
% Removes all animal names from the list on the server with the provided name.
%
% @param Registration_name the name of the server.
%
% @returns the {@type atom} _ok_.
%
% @end
remove_all(Registration_name) ->
  gen_server:call(Registration_name, remove_all).

%%% --------------------------
%%% Server Callbacks
%%% --------------------------

-spec handle_call({term(), atom()}, {pid(), _}, List :: atom()) ->
                   {reply, ok, List :: atom()} | {reply, {ok, List :: atom()}, List :: atom()}.
% @doc
% Deals with the requests made to {@type gen_server}. The available request types are:
% <ol>
%   <li>adding an animal, _O(1)_,</li>
%   <li>removing an animal, _O(n)_,</li>
%   <li>retrieving the list of animals, _O(1)_, and</li>
%   <li>removing all animals _O(1)_</li>
%   <li>stop, to elegantly stop the {@type gen_server} process _O(1)_</li>
% </ol>
%
% To stop the server the parameters are:
% <ol>
%   <li>stop, an atom,</li>
%   <li>From, the PID of the {@type gen_server} process, and</li>
%   <li>State, the current state of the {@type gen_server}.</li>
% </ol>
%
% @param Animal the name of the animal.
%
% @param Animals the list of animals in the zoo.
%
% @param From ignored.
%
% @returns the {@type atom} _ok_ or a 2-tuple with the {@type atom} _ok_ and the list of Animals.
%
% @end
handle_call({add, Animal}, _From, Animals) ->
  {reply, ok, [Animal] ++ Animals};
handle_call(list, _From, Animals) ->
  {reply, {ok, Animals}, Animals};
handle_call({remove, Animal}, _From, Animals) ->
  {reply, ok, lists:delete(Animal, Animals)};
handle_call(remove_all, _From, _Animals) ->
  {reply, ok, []}.

% @doc
% Initializes the server state.
%
% @end
init(State) ->
  {ok, State}.

% @doc
% Handles calls to the server but does not reply back to the calling process.
%
% @end
handle_cast(_Msg, State) ->
  {noreply, State}.

% @doc
% Handles other server calls or server time-outs.
%
% @end
handle_info(_Info, State) ->
  {noreply, State}.

% @doc
% Handles any clean-up before the server terminates.
%
% @end
terminate(_Reason, _State) ->
  ok.

% @doc
% For updating the internal state during upgrade/downgrade.
%
% @end
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

%%% --------------------------
%%% Unit Tests
%%% --------------------------

start_test() ->
  {setup,
   fun() -> nil end,
   fun() ->
      gen_server:stop(?MODULE),
      gen_server:stop(testing_name)
   end,
   [?assertMatch({ok, _}, start()), ?assertMatch({ok, _}, start(local, testing_name, []))]}.

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

