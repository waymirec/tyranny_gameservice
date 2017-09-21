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
    AuthSrvHello = {serverinfo_announce, {serverinfo_announce, start_link, []}, permanent, 2000, worker, [serverinfo_announce]},
    Children = [AuthSrvHello],
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.


%%====================================================================
%% Internal functions
%%====================================================================