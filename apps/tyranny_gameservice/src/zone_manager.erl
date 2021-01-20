-module(zone_manager).
-behaviour(gen_server).

%% api callbacks
-export([
  start_link/0,
  zones/0,
  which_zone/1,
  track/1,
  clear/1,
  broadcast/3
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

-include("zone.hrl").

-record(state, {
  zones = []          :: [#zone{}]
}).

%% API functions
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

zones() ->
  gen_server:call(?SERVER, zones).

which_zone({_X,_Y,_Z} = Coord) when is_float(_X) and is_float(_Y) and is_float(_Z) ->
  gen_server:call(?SERVER, {which_zone, Coord}).

track(Pid) when is_pid(Pid) ->
  gen_server:cast(?SERVER, {track, Pid}).

clear(Pid) when is_pid(Pid) ->
  gen_server:cast(?SERVER, {clear, Pid}).

broadcast({pid, Pid}, Message, Distance) ->
  gen_server:cast(?SERVER, {broadcast_from_pid, Pid, Message, Distance});

broadcast({vector, Vector}, Message, Distance) ->
  gen_server:cast(?SERVER, {broadcast_from_vector, Vector, Message, Distance}).

%% gen_server callbacks
init([]) ->
  Zones = [create_zone(X) || X <- config:key(<<"zones">>)],
  {ok, #state{ zones = Zones}, 0}.

handle_call({which_zone, Coord}, _From, #state{zones = Zones} = State) ->
  {reply, find_zone(Coord, Zones), State};

handle_call(zones, _From, #state{zones = Zones} = State) ->
  {reply, Zones, State}.

handle_cast({track, Pid}, #state{zones = Zones} = State) ->
  lists:foreach(fun(#zone{id = ZoneId}) -> tyranny_zone:delete_object(ZoneId, Pid) end, Zones),
  {ok, Coord} = gen_server:call(Pid, location),
  {ok, Zone} = find_zone(Coord, Zones),
  tyranny_zone:add_object(Zone#zone.id, Pid),
  {noreply, State};

handle_cast({clear, Pid}, #state{zones = Zones} = State) ->
  lists:foreach(fun(#zone{id = ZoneId}) -> tyranny_zone:delete_object(ZoneId, Pid) end, Zones),
  {noreply, State};

handle_cast({broadcast_from_pid, Pid, Message, Distance}, #state{zones = Zones} = State) ->
  {ok, Location} = gen_server:call(Pid, location),
  {ok, Zone} = find_zone(Location, Zones),
  lager:debug("Broadcasting to ~p.", [Zone#zone.id]),
  tyranny_zone:broadcast(Zone#zone.id, {pid, Pid}, Message, Distance),
  {noreply, State};

handle_cast({broadcast_from_vector, Location, Message, Distance}, #state{zones = Zones} = State) ->
  {ok, Zone} = find_zone(Location, Zones),
  lager:debug("Broadcasting to ~p.", [Zone#zone.id]),
  tyranny_zone:broadcast(Zone#zone.id, {vector3, Location}, Message, Distance),
  {noreply, State}.

handle_info(timeout, #state{ zones = Zones} = State) ->
  tyranny_zone_sup:start_link(),
  start_zones(Zones),
  game_event:add_handler(?SERVER, [game_object_spawned, game_object_destroyed]),
  {noreply, State};

handle_info({game_object_spawned, Pid}, State) ->
  track(Pid),
  {noreply, State};

handle_info({game_object_destroyed, Pid}, State) ->
  clear(Pid),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, _State) ->
  lager:debug("TERMINATING: ~w", [Reason]),
  game_event:delete_handler(self(), []),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal functions
start_zones([]) ->
  ok;

start_zones([#zone{id = ZoneId} | Rest]) ->
  case whereis(ZoneId) of
    undefined ->
      lager:debug("Starting zone ~p.", [ZoneId]),
      {ok, _Pid} = tyranny_zone_sup:start_child(ZoneId);
    _Pid -> ok
  end,
  start_zones(Rest).

create_zone({Id, {X, Y, Z}, {W, H}, Neighbors}) ->
  #zone{
    id = Id,
    top_left = #vector3{x=X, y=Y, z=Z},
    top_right = #vector3{x=X+W, y=Y, z=Z},
    bottom_left = #vector3{x=X, y=Y, z=Z+H},
    bottom_right = #vector3{x=X+W, y=Y, z=Z+H},
    neighbors = Neighbors
  }.

find_zone(Vector, Zones) ->
  case [Z || Z <- Zones, coord_in_zone(Z, Vector)] of
    [] -> unknown;
    [Z] -> {ok, Z}
  end.

coord_in_zone(#zone{top_left = #vector3{x=X1,z=Z1}, top_right = #vector3{x=X2}, bottom_left = #vector3{z=Z2}}=_Zone, #vector3{x=X,z=Z}) ->
  if
    X < X1 -> false;
    X > X2 -> false;
    Z < Z1 -> false;
    Z > Z2 -> false;
    true -> true
  end.
