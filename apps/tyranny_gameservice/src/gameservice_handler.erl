-module(gameservice_handler).
-behavior(ranch_protocol).
-behavior(gen_server).

%% Ranch Callbacks
-export([start_link/4,
  init/1
]).

%% Gen_Server Callbacks
-export([handle_cast/2,
  handle_call/3,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("opcodes.hrl").

-record(state, {
  ref                   :: ranch:ref(),
  client_id             :: binary(),
  socket                :: gen_tcp:socket(),
  transport             :: module(),
  socket_timeout = 5000 :: integer(),
  ping_timer            :: any(),
  ping_interval = 1000  :: integer()
}).

-type state() :: #state{}.

-define(config(Key, Opts), proplists:get_value(Key, Opts)).

-spec start_link(Ref :: ranch:ref(), Socket :: gen_udp:socket(), Transport :: module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
  %% start the process syncronously to avoid deadlock
  {ok, proc_lib:spawn_link(?MODULE, init, [[Ref, Socket, Transport, Opts]])}.

-spec init(Args :: list()) -> no_return().
init([Ref, Socket, Transport, Opts]) ->
  {ok, {IpAddress, Port}} = inet:peername(Socket),
  ClientId = list_to_binary(io_lib:format("~s:~p", [inet:ntoa(IpAddress), Port])),
  lager:info("[~s] Connection accepted from client ~s:~p", [ClientId, inet:ntoa(IpAddress), Port]),

  %% Perform any required state initialization here
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, false}, {packet, 4}]),

  {ok, Data} = Transport:recv(Socket, 0, 5000),
  <<?OP_IDENT:16, UserNameLength:16, UserName:UserNameLength/binary, TokenLength:16, Token:TokenLength/binary>> = Data,
  State = #state{ref = Ref, client_id = ClientId, socket = Socket, transport = Transport},
  lager:debug("[~s] Validating token [~s] for user ~s", [ClientId, Token, UserName]),
  case authtoken_manager:validate(UserName, Token) of
    true ->
      lager:debug("[~s] Token is valid", [ClientId]),
      SocketTimeout = ?config(socket_timeout, Opts),
      PingInterval = ?config(ping_interval, Opts),
      PingTimer = erlang:send_after(PingInterval, self(), {ping, 1}),
      State1 = State#state{socket_timeout = SocketTimeout,
        ping_interval = PingInterval,
        ping_timer = PingTimer},
      ok = Transport:setopts(Socket, [{active, once}]),
      gen_server:enter_loop(?MODULE, [], State1, SocketTimeout);
    false ->
      lager:debug("[~s] Token is not valid: ~p", [ClientId, Token]),
      Transport:close(Socket),
      ok
  end.

-spec handle_info(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_info({tcp, Socket, <<?HDR_PING, Count:32>>}, State) ->
  #state{client_id = ClientId, transport = Transport, socket = Socket, socket_timeout = SocketTimeout} = State,
  lager:debug("[~s] Received ping(~p)!", [ClientId, Count]),
  ok = gen_tcp:send(Socket, <<?HDR_PONG, Count:32>>),
  ok = Transport:setopts(Socket, [{active, once}]),
  {noreply, State, SocketTimeout};

handle_info({tcp, Socket, <<?HDR_PONG, Count:32>>}, State) ->
  #state{client_id = ClientId, socket = Socket, transport = Transport} = State,
  Transport:setopts(Socket, [{active, once}]),
  lager:debug("[~s] Received pong(~p)!", [ClientId, Count]),
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
  lager:debug("[~s] Sending ping(~p)", [ClientId, Count]),
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
handle_cast(_Msg, #state{client_id = ClientId} = State) ->
  lager:debug("[~s] Received unexpected cast", [ClientId]),
  {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: state()) -> ok.
terminate(_Reason, State) ->
  #state{client_id = ClientId, transport = Transport, socket = Socket, ping_timer = PingTimer} = State,
  lager:debug("[~s] Terminating handler", [ClientId]),
  timer:cancel(PingTimer),
  Transport:close(Socket),
  ok.

-spec code_change(OldVsn :: {down, term()} | term(), State :: gen_server:state(), Extra :: term()) -> {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

