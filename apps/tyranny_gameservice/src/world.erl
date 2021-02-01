-module(world).
-behaviour(gen_server).
-author("waymirec").

%% API
-export([
  start_link/0,
  player_enter_world/3,
  world_entity_disco/1
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
  group                     :: binary(),
  port                      :: integer(),
  socket                    :: gen_udp:socket(),
  players = dict:new()
}).

-include_lib("kernel/include/logger.hrl").
-include("world_opcodes.hrl").
-include("game_opcodes.hrl").
-include("vector3.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

player_enter_world(Pid, PlayerGuid, Vector) ->
  gen_server:cast(?SERVER, {player_enter_world, Pid, PlayerGuid, Vector}).

world_entity_disco(PlayerGuid) ->
  gen_server:cast(?SERVER, {world_entity_disco_req, PlayerGuid}).

%% gen_server callbacks
init([]) ->
  Address = config:key(<<"world.address">>),
  Group = config:key(<<"world.mcast_group">>),
  Port = config:key(<<"world.mcast_port">>),

  {ok, Socket} = gen_udp:open(Port, [binary, {active, once}, {reuseaddr, true},{ip, Address}]),
  inet:setopts(Socket,[{add_membership,{Group,Address}}]),
  {ok, #state{group = Group, port = Port, socket = Socket}, 0}.

handle_call(_Message, _From, State) ->
  {ok, reply, State}.

handle_cast({player_enter_world, Pid, PlayerGuid, #vector3{x=X,y=Y,z=Z}=_Vector}, #state{socket = Socket, port = Port, players = Players} = State) ->
  EnterWorldMessage = <<?WORLD_HDR_ENTER_WORLD, PlayerGuid/binary, X:32/float, Y:32/float, Z:32/float>>,
  gen_udp:send(Socket, {239,0,1,1}, Port, EnterWorldMessage),
  ?LOG_INFO(#{what => "packet_out", opcode => ?WORLD_OP_ENTER_WORLD, dst => #{ip4 => inet:parse_ipv4_address("239.0.1.1")}}),
  {noreply, State#state{players = dict:store(PlayerGuid, Pid, Players)}};

handle_cast({world_entity_disco_req, PlayerGuid}, #state{socket = Socket, port = Port} = State) ->
  gen_udp:send(Socket, {239,0,1,1}, Port, <<?WORLD_HDR_WORLD_ENTITY_DISCO, PlayerGuid/binary>>),
  ?LOG_INFO(#{what => "packet_out", opcode => ?WORLD_OP_WORLD_ENTITY_DISCO, dst => #{ip4 => inet:parse_ipv4_address("239.0.1.1")}}),
  {noreply, State};

handle_cast(test, #state{socket = Socket, group = _Group, port = Port} = State) ->
  gen_udp:send(Socket, {239,0,1,1}, Port, <<11:16, 7:16, <<"testing">>/binary>>),
  {noreply, State};

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info({udp, Socket, Ip, _InPortNo, <<?WORLD_HDR_SPAWN_WORLD_ENTITY, PlayerGuid:16, EntityGuid:16, X:32/float, Y:32/float, Z:32/float>>}, #state{socket = Socket, players = Players} = State) ->
  ?LOG_INFO(#{what => "packet_in", opcode => ?WORLD_OP_SPAWN_WORLD_ENTITY, src => #{ip4 => Ip}}),
  PlayerPid = dict:fetch(PlayerGuid, Players),
  SpawnMessage = <<?GAME_HDR_SPAWN_WORLD_ENTITY, EntityGuid/binary, X:32/float, Y:32/float, Z:32/float>>,
  player:send(PlayerPid, SpawnMessage),
  ok = gen_udp:setopts(Socket, [{active, once}]),
  {noreply, State};

handle_info({udp, Socket, _IP, _InPortNo, <<?WORLD_HDR_NOOP, _Ignored:8>>}, #state{socket = Socket} = State) ->
  ok = gen_udp:setopts(Socket, [{active, once}]),
  {noreply, State};

handle_info(timeout, #state{ } = State) ->
  {noreply, State};

%% Event callbacks
handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  game_event:delete_handler(self(), []),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.