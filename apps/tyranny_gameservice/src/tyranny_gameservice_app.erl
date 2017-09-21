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
    config:start_link(),

    application:start(ranch),
    {ok, _} = ranch:start_listener(tyranny_gameservice,
				   ranch_tcp,
				   [ {port, 12346}, 
				     {num_acceptors, config:num_acceptors()}, 
				     {max_connections, config:max_connections()}
 				   ],
				   authservice_handler,
				   []),

    tyranny_gameservice_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
