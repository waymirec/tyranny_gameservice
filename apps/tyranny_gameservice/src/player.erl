-module(player).
-behavior(gen_server).

%% API
-export([
  start_link/4,
  send/2
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).
-define(config(Key, Opts), proplists:get_value(Key, Opts)).

-include_lib("kernel/include/logger.hrl").
-include("types.hrl").
-include("vector3.hrl").
-include("game_opcodes.hrl").

-record(state, {
  game_id                                           :: binary(),
  socket                                            :: gen_tcp:socket(),
  socket_timeout = 5000                             :: integer(),
  transport                                         :: module(),
  handler                                           :: pid(),
  location = #vector3{x=0.0,y=0.0,z=0.0}            :: vector3()
}).
-type state() :: #state{}.

%% API functions
start_link(GameId, Transport, Socket, Opts) ->
  gen_server:start_link(?MODULE, [GameId, Transport, Socket, Opts], []).

send(Pid, Message) ->
  gen_server:cast(Pid, {send_message, Message}).

%% gen_server callbacks
init([GameId, Transport, Socket, _Opts]) ->
  logger:update_process_metadata(#{correlation_id => uuid:to_list(GameId)}),

  X=0,Y=0,Z=0,
  ok = Transport:send(Socket, <<?GAME_HDR_ENTER_WORLD, X:32/float, Y:32/float, Z:32/float>>),
  ?LOG_INFO(#{what => "packet_out", opcode => ?GAME_OP_ENTER_WORLD}),

  world:player_enter_world(self(), GameId, #vector3{x=X,y=Y,z=Z}),

  ok = Transport:setopts(Socket, [{active, once}]),
  {ok, #state{game_id=GameId, transport = Transport, socket = Socket}}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()}, State :: state()) -> Result :: {reply, Reply :: term(), NewState :: state()}.
handle_call(Request, _From, State) ->
  ?LOG_DEBUG(#{what => "message_in", result => "unexpected_call", details => Request}),
  {reply, ok, State}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_cast({send_message, <<OpCode:?GAME_OPCODE_LEN, _Rest/binary>> = Message}, #state{transport = Transport, socket = Socket} = State) ->
  ok = Transport:send(Socket, Message),
  ?LOG_INFO(#{what => "packet_out", opcode => OpCode}),
  {noreply, State};

handle_cast(Msg, State) ->
  ?LOG_DEBUG(#{what => "message_in", result => "unexpected_cast", details => Msg}),
  {noreply, State}.

-spec handle_info(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_info({tcp, Socket, <<?GAME_HDR_NOOP, _Ignored:8>>}, State) ->
  ?LOG_INFO(#{what => "packet_in", opcode => ?GAME_OP_NOOP}),
  #state{transport = Transport, socket = Socket, socket_timeout = SocketTimeout} = State,
  ok = Transport:setopts(Socket, [{active, once}]),
  {noreply, State, SocketTimeout};

handle_info({tcp, _Socket, <<?GAME_HDR_WORLD_ENTITY_DISCO>>}, #state{game_id = GameId} = State) ->
  ?LOG_INFO(#{what => "packet_in", opcode => ?GAME_OP_WORLD_ENTITY_DISCO}),
  world:world_entity_disco(GameId),
  {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
  ?LOG_DEBUG(#{what => "network", result => "closed"}),
  {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
  ?LOG_DEBUG(#{what => "network", result => "error", details => Reason}),
  {stop, Reason, State};

handle_info(timeout, State) ->
  ?LOG_DEBUG(#{what => "network", result => "timeout"}),
  {stop, normal, State};

handle_info(Info, State) ->
  #state{transport = Transport, socket = Socket} = State,
  ?LOG_DEBUG(#{what => "message_in", result => "unexpected_info", details => Info}),
  Transport:setopts(Socket, [{active, once}]),
  {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: state()) -> ok.
terminate(_Reason, State) ->
  #state{transport = Transport, socket = Socket} = State,
  ?LOG_DEBUG(#{what => "process", result => "terminate"}),
  Transport:close(Socket),
  ok.

-spec code_change(OldVsn :: {down, term()} | term(), State :: gen_server:state(), Extra :: term()) -> {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
