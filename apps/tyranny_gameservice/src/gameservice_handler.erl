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

-include("opcodes.hrl").
-include("vector3.hrl").

-define(SERVER, ?MODULE).

-record(state, {
  ref                   :: ranch:ref(),
  client_id             :: binary(),
  client_uuid           :: binary(),
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
  {ok, {IpAddress, Port}} = inet:peername(Socket),
  ClientId = list_to_binary(io_lib:format("~s:~p", [inet:ntoa(IpAddress), Port])),
  ClientUuid = uuid:create(),

  lager:info("[~s] Connection accepted from client ~s:~p", [ClientId, inet:ntoa(IpAddress), Port]),

  %% Perform any required state initialization here
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, false}, {packet, 4}]),

  {ok, Data} = Transport:recv(Socket, 0, 5000),
  <<?OP_IDENT:16, UserNameLength:16, UserName:UserNameLength/binary, TokenLength:16, Token:TokenLength/binary>> = Data,
  State = #state
  {
    ref = Ref,
    client_id = ClientId,
    client_uuid = ClientUuid,
    socket = Socket,
    transport = Transport
  },

  lager:debug("[~s] Validating token [~s] for user ~s", [ClientId, Token, UserName]),
  case authtoken_manager:validate(UserName, Token) of
    true ->
      lager:debug("[~s] Token is valid", [ClientId]),

      ok = Transport:send(Socket,<<?HDR_HELLO, ClientUuid/binary>>),

      {ok, <<?HDR_READY>>} = Transport:recv(Socket, 0, 5000),

      {ok, Player} = player:start_link(ClientId, ClientUuid, self()),
      {ok, #vector3{x=X,y=Y,z=Z}} = player:location(Player),

      ok = gen_tcp:send(Socket, <<?HDR_ENTER_WORLD, X:32/float, Y:32/float, Z:32/float>>),
      game_event:game_object_spawned(Player),

      SpawnMessage = <<?HDR_SPAWN_GO, ClientUuid/binary, X:32/float, Y:32/float, Z:32/float>>,
      zone_manager:broadcast({pid, Player}, SpawnMessage, 100.0),

      ok = Transport:setopts(Socket, [{active, once}]),
      PingInterval = ?config(ping_interval, Opts),
      PingTimer = erlang:send_after(PingInterval, self(), {ping, 1}),
      SocketTimeout = ?config(socket_timeout, Opts),

      State1 = State#state
      {
        player_pid = Player,
        socket_timeout = SocketTimeout,
        ping_interval = PingInterval,
        ping_timer = PingTimer
      },

      gen_server:enter_loop(?MODULE, [], State1, SocketTimeout);

    false ->
      lager:debug("[~s] Token is not valid: ~p", [ClientId, Token]),
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
  #state{client_id = ClientId, transport = Transport, socket = Socket, socket_timeout = SocketTimeout} = State,
  % lager:debug("[~s] Received ping(~p)!", [ClientId, Count]),
  ok = gen_tcp:send(Socket, <<?HDR_PONG, Count:32>>),
  ok = Transport:setopts(Socket, [{active, once}]),
  {noreply, State, SocketTimeout};

handle_info({tcp, Socket, <<?HDR_PONG, Count:32>>}, State) ->
  #state{client_id = ClientId, socket = Socket, transport = Transport} = State,
  Transport:setopts(Socket, [{active, once}]),
  % lager:debug("[~s] Received pong(~p)!", [ClientId, Count]),
  {noreply, State};

handle_info({tcp, Socket, <<?HDR_MOVE, Guid:128, X1:32/float, Y1:32/float, Z1:32/float, X2:32/float, Y2:32/float, Z2:32/float>> = Message}, State) ->
  #state{client_id = ClientId, socket = Socket, transport = Transport, player_pid = Player} = State,
  Transport:setopts(Socket, [{active, once}]),
  lager:debug("[~s] Received Move: GUID=~p, From = (~p, ~p, ~p) To = (~p, ~p, ~p)", [ClientId, Guid, X1, Y1, Z1, X2, Y2, Z2]),
  zone_manager:broadcast({pid, Player}, Message, 100.0),
  {noreply, State};

handle_info({tcp_closed, _Socket}, #state{client_id = ClientId} = State) ->
  lager:debug("[~s] Socket closed", [ClientId]),
  {stop, normal, State};

handle_info({tcp_error, _, Reason}, #state{client_id = ClientId} = State) ->
  lager:debug("[~s] Error: ~s", [ClientId, Reason]),
  {stop, Reason, State};

handle_info(timeout, #state{client_id = ClientId} = State) ->
  lager:debug("[~s] Timeout!", [ClientId]),
  {stop, normal, State};

handle_info({ping, Count}, State) ->
  #state{client_id = ClientId, socket = Socket, transport = Transport, socket_timeout = SocketTimeout, ping_interval = PingInterval} = State,
  % lager:debug("[~s] Sending ping(~p)", [ClientId, Count]),
  Transport:send(Socket, <<?HDR_PING, Count:32>>),
  NewPingTimer = erlang:send_after(PingInterval, self(), {ping, Count + 1}),
  {noreply, State#state{ping_timer = NewPingTimer}, SocketTimeout};

handle_info(Info, State) ->
  #state{client_id = ClientId, transport = Transport, socket = Socket} = State,
  lager:debug("[~s] Received unexpected info: ~p", [ClientId, Info]),
  Transport:setopts(Socket, [{active, once}]),
  {noreply, State}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()}, State :: state()) -> Result :: {reply, Reply :: term(), NewState :: state()}.
handle_call(_Request, _From, #state{client_id = ClientId} = State) ->
  lager:debug("[~s] Received unexpected call", [ClientId]),
  {reply, ok, State}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_cast({send_message, Message}, #state{socket = Socket} = State) ->
  ok = gen_tcp:send(Socket, Message),
  {noreply, State};

handle_cast(_Msg, #state{client_id = ClientId} = State) ->
  lager:debug("[~s] Received unexpected cast", [ClientId]),
  {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: state()) -> ok.
terminate(_Reason, State) ->
  #state{client_id = ClientId, client_uuid = ClientUuid, transport = Transport, socket = Socket, player_pid = Player, ping_timer = PingTimer} = State,
  lager:debug("[~s] Terminating handler", [ClientId]),
  timer:cancel(PingTimer),

  game_event:game_object_destroyed(Player),
  {ok, Location} = player:location(Player),
  DestroyMessage = <<?HDR_DESTROY_GO, ClientUuid/binary>>,
  zone_manager:broadcast({vector, Location}, DestroyMessage, 100.0),

  Transport:close(Socket),
  ok.

-spec code_change(OldVsn :: {down, term()} | term(), State :: gen_server:state(), Extra :: term()) -> {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
