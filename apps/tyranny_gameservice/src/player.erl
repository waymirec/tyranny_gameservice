-module(player).
-behavior(gen_server).

%% API
-export([
  start_link/3,
  location/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  handle_event/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).

-include("types.hrl").
-include("vector3.hrl").

-record(state, {
  id                                                :: binary(),
  uuid                                              :: binary(),
  handler                                           :: pid(),
  location = #vector3{x=0.0,y=0.0,z=0.0}            :: vector3()
}).

%% API functions
start_link(Id, Uuid, Handler) ->
  gen_server:start_link(?MODULE, [Id, Uuid, Handler], []).

location(Pid) ->
  gen_server:call(Pid, location).

%% gen_server callbacks
init([Id, Uuid, Handler]) ->
  {ok, #state{id=Id, uuid=Uuid, handler=Handler}}.

handle_call(location, _From, State) ->
  {reply, {ok, State#state.location}, State};

handle_call(_Message, _From, State) ->
  {reply, ok, State}.

handle_cast({send_message, Message}, #state{uuid = Uuid, handler = Handler} = State) ->
  lager:debug("[~s] Sending message: ~w", [Uuid, Message]),
  gen_server:cast(Handler, {send_message, Message}),
  {noreply, State};

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

handle_event({player_enter_world, _Pid}, State) ->
  lager:debug("We are free!!", []),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.