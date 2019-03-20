%%%-------------------------------------------------------------------
%% @doc tyranny_gameservice public API
%% @end
%%%-------------------------------------------------------------------

-module(tyranny_gameservice_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  lager:start(),
  application:start(os_mon),
  application:start(ranch),
  {ok, _} = ranch:start_listener(tyranny_gameservice,
    ranch_tcp,
    [{port, config:key(<<"listener.port">>)},
      {num_acceptors, config:key(<<"listener.num_acceptors">>)},
      {max_connections, config:key(<<"listener.max_connections">>)}
    ],
    gameservice_handler,
    [{socket_timeout, config:key(<<"listener.socket_timeout">>)},
      {ping_interval, config:key(<<"ping_interval">>)}
    ]),

  tyranny_gameservice_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
