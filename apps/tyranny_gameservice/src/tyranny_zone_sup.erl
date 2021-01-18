-module(tyranny_zone_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/1,
  start_child/1
]).

%% Supervisor callbacks
-export([
  init/1
]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Zones) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Zones]).

start_child(ZoneId) ->
  supervisor:start_child(?SERVER, [ZoneId]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Zones]) ->
  ZoneSpec = {tyranny_zone, {tyranny_zone, start_link, [Zones]}, permanent, 2000, worker, [tyranny_zone]},
  Children = [ZoneSpec],
  RestartStrategy = {simple_one_for_one, 10, 10},
  {ok, {RestartStrategy, Children}}.