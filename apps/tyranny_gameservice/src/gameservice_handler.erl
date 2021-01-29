-module(gameservice_handler).
-behavior(ranch_protocol).
-behavior(gen_server).

%% API callbacks
-export([
  send_message/2
]).

%% Ranch Callbacks
-export([
  start_link/4,
  init/1
]).

%% Gen_Server Callbacks
-export([
  handle_cast/2,
  handle_call/3,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include_lib("kernel/include/logger.hrl").
-include("opcodes.hrl").
-include("vector3.hrl").

-define(SERVER, ?MODULE).

-record(state, {
  ref                   :: ranch:ref(),
  client_id             :: binary(),
  username              :: binary(),
  socket                :: gen_tcp:socket(),
  transport             :: module(),
  socket_timeout = 5000 :: integer(),
  player_pid            :: pid(),
  ping_timer            :: any(),
  ping_interval = 1000  :: integer()
}).

-type state() :: #state{}.

-define(config(Key, Opts), proplists:get_value(Key, Opts)).

%% API callbacks

send_message(Pid, Message) when is_binary(Message) ->
  gen_server:cast(Pid, {send_message, Message}).

%% Ranch callbacks

-spec start_link(Ref :: ranch:ref(), Socket :: gen_tcp:socket(), Transport :: module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
  %% start the process synchronously to avoid deadlock
  {ok, proc_lib:spawn_link(?MODULE, init, [[Ref, Socket, Transport, Opts]])}.

-spec init(Args :: list()) -> no_return().
init([Ref, Socket, Transport, Opts]) ->
  State = accept_connection(Ref, Socket, Transport, Opts),
  {Token, #state{}=State1} = wait_for_ident(State),

  case authtoken_manager:validate(State1#state.username, Token) of
    true ->
      State2 = handshake(State1),
      State3 = start_player_process(State2),
      State4 = setup_ping(State3),
      enter_loop(State4);
    false ->
      ?LOG_INFO(#{what => "authentication", result=>"failure", details => Token}),
      Transport:close(Socket),
      ok
  end.

%% gen_server callbacks

-spec handle_info(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_info({tcp, Socket, <<?HDR_NOOP, _Ignored:8>>}, State) ->
  #state{transport = Transport, socket = Socket, socket_timeout = SocketTimeout} = State,
  ok = Transport:setopts(Socket, [{active, once}]),
  {noreply, State, SocketTimeout};

handle_info({tcp, Socket, <<?HDR_PING, Count:32>>}, State) ->
  #state{transport = Transport, socket = Socket, socket_timeout = SocketTimeout} = State,
  ok = gen_tcp:send(Socket, <<?HDR_PONG, Count:32>>),
  ok = Transport:setopts(Socket, [{active, once}]),
  {noreply, State, SocketTimeout};

handle_info({tcp, Socket, <<?HDR_PONG, _Count:32>>}, State) ->
  #state{socket = Socket, transport = Transport} = State,
  Transport:setopts(Socket, [{active, once}]),
  {noreply, State};

handle_info({tcp, Socket, <<?HDR_MOVE_WORLD_ENTITY, X1:32/float, Y1:32/float, Z1:32/float, X2:32/float, Y2:32/float, Z2:32/float>>}, State) ->
  #state{socket = Socket, transport = Transport, player_pid = Player, client_id = ClientId} = State,
  Transport:setopts(Socket, [{active, once}]),

  ?LOG_DEBUG(#{what => "request:move", player_pid => Player, src => #{vector => #vector3{x=X1,y=Y1,z=Z1}}, dst => #{vector => #vector3{x=X2,y=Y2,z=Z2}}}),
  Message = <<?HDR_MOVE_WORLD_ENTITY, ClientId/binary, X1:32/float, Y1:32/float, Z1:32/float, X2:32/float, Y2:32/float, Z2:32/float>>,
  megaphone:broadcast(Message, Player, 100.0),

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

handle_info({ping, Count}, State) ->
  #state{socket = Socket, transport = Transport, socket_timeout = SocketTimeout, ping_interval = PingInterval} = State,
  Transport:send(Socket, <<?HDR_PING, Count:32>>),
  NewPingTimer = erlang:send_after(PingInterval, self(), {ping, Count + 1}),
  {noreply, State#state{ping_timer = NewPingTimer}, SocketTimeout};

handle_info(Info, State) ->
  #state{transport = Transport, socket = Socket} = State,
  ?LOG_DEBUG(#{what => "process", result => "unexpected_info", details => Info}),
  Transport:setopts(Socket, [{active, once}]),
  {noreply, State}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()}, State :: state()) -> Result :: {reply, Reply :: term(), NewState :: state()}.
handle_call(Request, _From, State) ->
  ?LOG_DEBUG(#{what => "process", result => "unexpected_call", details => Request}),
  {reply, ok, State}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_cast({send_message, Message}, #state{transport = Transport, socket = Socket} = State) ->
  ok = Transport:send(Socket, Message),
  {noreply, State};

handle_cast(Msg, State) ->
  ?LOG_DEBUG(#{what => "process", result => "unexpected_cast", details => Msg}),
  {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: state()) -> ok.
terminate(_Reason, State) ->
  #state{transport = Transport, socket = Socket, player_pid = Player, ping_timer = PingTimer} = State,
  ?LOG_DEBUG(#{what => "process", result => "terminate"}),
  timer:cancel(PingTimer),

  game_event:game_object_destroyed(Player),
  %{ok, Location} = game_object:location(Player),
  %DestroyMessage = <<?HDR_DESTROY_GO, ClientId/binary>>,
  %megaphone:broadcast(DestroyMessage, Location, 100.0),

  Transport:close(Socket),
  ok.

-spec code_change(OldVsn :: {down, term()} | term(), State :: gen_server:state(), Extra :: term()) -> {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
accept_connection(Ref, Socket, Transport, Opts) ->
  ClientId = uuid:create(),

  logger:update_process_metadata(#{correlation_id => uuid:to_list(ClientId)}),

  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, false}, {packet, 4}]),

  {ok, {IpAddress, Port}} = inet:peername(Socket),

  ?LOG_INFO(#{what => "connect", result => "success", src => #{ip4 => inet:ntoa(IpAddress), port=> Port}}),

  #state{
    ref = Ref,
    transport = Transport,
    socket = Socket,
    client_id = ClientId,
    socket_timeout = ?config(socket_timeout, Opts),
    ping_interval = ?config(ping_interval, Opts)
  }.

wait_for_ident(#state{transport = Transport, socket = Socket} = State) ->
  {ok, Data} = Transport:recv(Socket, 0, 5000),
  <<?OP_IDENT:16, UserNameLength:16, UserName:UserNameLength/binary, TokenLength:16, Token:TokenLength/binary>> = Data,
  {Token, State#state{username = UserName}}.

handshake(#state{client_id = ClientId, socket = Socket, transport = Transport} = State) ->
  ok = Transport:send(Socket,<<?HDR_HELLO, ClientId/binary>>),
  {ok, <<?HDR_READY>>} = Transport:recv(Socket, 0, 5000),
  State.

start_player_process(#state{client_id = ClientId, transport = Transport, socket = Socket} = State) ->
  {ok, Pid} = player:start_link(ClientId, self()),

  {ok, #vector3{x=X,y=Y,z=Z}=Vector} = player:location(Pid),
  ok = Transport:send(Socket, <<?HDR_ENTER_WORLD, X:32/float, Y:32/float, Z:32/float>>),
  game_event:game_object_spawned(Pid, Vector),

  SpawnMessage = <<?HDR_SPAWN_WORLD_ENTITY, ClientId/binary, X:32/float, Y:32/float, Z:32/float>>,
  megaphone:broadcast(SpawnMessage, Pid, 100.0),

  NearbyObjects = megaphone:discover_objects(Pid, 100.0),
  discover_objects(NearbyObjects, State),

  State#state{player_pid = Pid}.

setup_ping(#state{ping_interval = PingInterval} = State) ->
  State#state{ping_timer = erlang:send_after(PingInterval, self(), {ping, 1})}.

enter_loop(#state{transport = Transport, socket = Socket, socket_timeout = SocketTimeout} = State) ->
  ok = Transport:setopts(Socket, [{active, once}]),
  gen_server:enter_loop(?MODULE, [], State, SocketTimeout),
  ok.

discover_objects([Object | Objects], #state{transport = Transport, socket = Socket} = State) ->
  {ok, Id} = player:id(Object),
  {ok, #vector3{x=X,y=Y,z=Z}} = player:location(Object),
  SpawnMessage = <<?HDR_SPAWN_WORLD_ENTITY, Id/binary, X:32/float, Y:32/float, Z:32/float>>,
  Transport:send(Socket, SpawnMessage),
  discover_objects(Objects, State);

discover_objects([], _State) ->
  ok.