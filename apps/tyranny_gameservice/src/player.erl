-module(player).
-behavior(gen_server).

%% API
-export([
  start_link/2,
  location/1,
  id/1
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

-include_lib("kernel/include/logger.hrl").
-include("types.hrl").
-include("vector3.hrl").

-record(state, {
  id                                                :: binary(),
  handler                                           :: pid(),
  location = #vector3{x=0.0,y=0.0,z=0.0}            :: vector3()
}).

%% API functions
start_link(Id, Handler) ->
  gen_server:start_link(?MODULE, [Id, Handler], []).

location(Pid) -> gen_server:call(Pid, {get,location}).
id(Pid) -> gen_server:call(Pid, {get, id}).

%% gen_server callbacks
init([Id, Handler]) ->
  logger:update_process_metadata(#{correlation_id => uuid:to_list(Id)}),
  {ok, #state{id=Id, handler=Handler}}.

handle_call({get, location}, _From, #state{location=Location} = State) -> {reply, {ok, Location}, State};
handle_call({get, id}, _From, #state{id = Id} = State) -> {reply, {ok, Id}, State};

handle_call(_Message, _From, State) -> {reply, ok, State}.

handle_cast({send_message, Message}, #state{handler = Handler} = State) ->
  ?LOG_DEBUG(#{details => "sending message to self"}),
  gen_server:cast(Handler, {send_message, Message}),
  {noreply, State};

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

handle_event({player_enter_world, _Pid}, State) ->
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.