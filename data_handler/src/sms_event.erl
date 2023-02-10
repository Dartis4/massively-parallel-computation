-module(sms_event).
-behaviour(gen_event).
 
-export([init/1, handle_event/2, handle_call/2, handle_info/2]).
 
init([]) ->
{ok, []}.
 
handle_event({error, _Message}, State) ->
io:format("Error BAD_SMS."),
{ok, State};

handle_event({success, _Message}, State) ->
io:format("Success GOOD_SMS."),
{ok, State}.
 
handle_call(_, State) ->
{ok, ok, State}.
 
handle_info(_, State) ->
{ok, State}.