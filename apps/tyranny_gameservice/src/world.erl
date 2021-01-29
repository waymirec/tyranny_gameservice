-module(world).
-behaviour(gen_server).
-author("waymirec").

%% API
-export([
  start_link/0
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
  group                 :: binary(),
  port                  :: integer(),
  socket                :: gen_udp:socket()
}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
  Addr = config:key(<<"world.address">>),
  Group = config:key(<<"world.mcast_group">>),
  Port = config:key(<<"world.mcast_port">>),

  {ok, Socket} = gen_udp:open(Port, [binary, {active, false}, {reuseaddr, true},{ip, Addr}]),
  inet:setopts(Socket,[{add_membership,{Group,Addr}}]),
  {ok, #state{group = Group, port = Port, socket = Socket}, 0}.

handle_call(_Message, _From, State) ->
  {ok, reply, State}.

handle_cast(test, #state{socket = Socket, group = _Group, port = Port} = State) ->
  gen_udp:send(Socket, {239,0,1,1}, Port, <<11:16, 7:16, <<"testing">>/binary>>),
  {noreply, State};

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(timeout, #state{ } = State) ->
  {noreply, State};

%% Event callbacks
handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  game_event:delete_handler(self(), []),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.