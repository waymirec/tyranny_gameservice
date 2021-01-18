%%%-------------------------------------------------------------------
%%% @author waymirec
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jan 2021 10:03 PM
%%%-------------------------------------------------------------------
-module(tyranny_zone).
-author("waymirec").

-behavior(gen_server).

%% API callbacks

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("zone.hrl").

-record(state, {
  zone                  :: #zone{},
  users = []            :: [pid()],
  neighbors = []        :: [pid()]
}).

%% API functions

%% gen_server functions
init([Zones, ZoneId]) ->
  [#zone{id=ZoneId} = Zone] = Zones,
  State = #state{zone = Zone},
  {ok, State, 0}.

handle_call(_Message, _From, State) ->
  {reply, ok, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(timeout, #state{zone = #zone{id = ZoneId}} = State) ->
  register(ZoneId, self()),
  {noreply, State};

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.