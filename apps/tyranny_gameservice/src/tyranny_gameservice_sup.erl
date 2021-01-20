%%%-------------------------------------------------------------------
%% @doc tyranny_gameservice top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tyranny_gameservice_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  EventManager = {game_event, {game_event, start_link, []}, permanent, 2000, worker, [game_event]},
  GameServicePublisher = {gameservice_publisher, {gameservice_publisher, start_link, []}, permanent, 2000, worker, [gameservice_publisher]},
  AuthTokenManager = {authtoken_manager, {authtoken_manager, start_link, []}, permanent, 2000, worker, [authtoken_manager]},
  ZoneManager = {zone_manager, {zone_manager, start_link, []}, permanent, 2000, worker, [zone_manager]},
  Children = [EventManager, GameServicePublisher, AuthTokenManager, ZoneManager],
  RestartStrategy = {one_for_one, 10, 10},
  {ok, {RestartStrategy, Children}}.


%%====================================================================
%% Internal functions
%%====================================================================
