-module(gameservice_handler).
-behavior(ranch_protocol).

%% Ranch Callbacks
-export([
  start_link/4,
  init/1
]).

-include_lib("kernel/include/logger.hrl").
-include("game_opcodes.hrl").
-include("vector3.hrl").

-define(SERVER, ?MODULE).

-record(state, {
  ref                   :: ranch:ref(),
  client_id             :: binary(),
  username              :: binary(),
  socket                :: gen_tcp:socket(),
  transport             :: module(),
  player_pid            :: pid()
}).

%% Ranch callbacks

-spec start_link(Ref :: ranch:ref(), Socket :: gen_tcp:socket(), Transport :: module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
  %% start the process synchronously to avoid deadlock
  {ok, proc_lib:spawn_link(?MODULE, init, [[Ref, Socket, Transport, Opts]])}.

-spec init(Args :: list()) -> no_return().
init([Ref, Socket, Transport, Opts]) ->
  State = accept_connection(Ref, Socket, Transport),
  {Token, #state{}=State1} = wait_for_ident(State),

  case authtoken_manager:validate(State1#state.username, Token) of
    true ->
      State2 = handshake(State1),
      start_player_process(Opts, State2);
    false ->
      ?LOG_INFO(#{what => "authentication", result=>"failure", details => Token}),
      Transport:close(Socket),
      ok
  end.


%% Internal functions
accept_connection(Ref, Socket, Transport) ->
  ClientId = uuid:create(),

  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, false}, {packet, 4}]),
  {ok, {IpAddress, Port}} = inet:peername(Socket),

  ?LOG_INFO(#{id => uuid:to_list(ClientId), what => "connect", result => "success", src => #{ip4 => inet:ntoa(IpAddress), port=> Port}}),

  logger:update_process_metadata(#{correlation_id => uuid:to_list(ClientId)}),

  #state{
    ref = Ref,
    transport = Transport,
    socket = Socket,
    client_id = ClientId
  }.

wait_for_ident(#state{transport = Transport, socket = Socket} = State) ->
  {ok, Data} = Transport:recv(Socket, 0, 5000),
  <<?GAME_OP_IDENT:16, UserNameLength:16, UserName:UserNameLength/binary, TokenLength:16, Token:TokenLength/binary>> = Data,
  ?LOG_INFO(#{what => "packet_in", opcode => ?GAME_OP_IDENT}),
  {Token, State#state{username = UserName}}.

handshake(#state{client_id = ClientId, socket = Socket, transport = Transport} = State) ->
  ok = Transport:send(Socket,<<?GAME_HDR_HELLO, ClientId/binary>>),
  ?LOG_INFO(#{what => "packet_out", opcode => ?GAME_OP_HELLO}),
  {ok, <<?GAME_HDR_READY>>} = Transport:recv(Socket, 0, 5000),
  ?LOG_INFO(#{what => "packet_in", opcode => ?GAME_OP_READY}),
  State.

start_player_process(Opts, #state{client_id = ClientId, transport = Transport, socket = Socket} = State) ->
  {ok, Pid} = player:start_link(ClientId, Transport, Socket, Opts),
  gen_tcp:controlling_process(Socket, Pid),
  State#state{player_pid = Pid}.
