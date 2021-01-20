-module(tyranny_zone_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/0,
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

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(ZoneId) ->
  supervisor:start_child(?SERVER, [ZoneId]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  ZoneSpec = {tyranny_zone, {tyranny_zone, start_link, []}, permanent, 2000, worker, [tyranny_zone]},
  Children = [ZoneSpec],
  RestartStrategy = {simple_one_for_one, 10, 10},
  {ok, {RestartStrategy, Children}}.