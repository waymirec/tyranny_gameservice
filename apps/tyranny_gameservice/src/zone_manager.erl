-module(zone_manager).
-behaviour(gen_server).

%% api callbacks
-export([
  start_link/1,
  zones/0,
  get/1,
  locate/1
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
start_link(ZoneData) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ZoneData], []).

zones() ->
  gen_server:call(?SERVER, zones).

locate({_,_,_} = Coord) ->
  gen_server:call(?SERVER, {locate, Coord}).

get(ZoneId) when is_atom(ZoneId) ->
  case whereis(ZoneId) of
    undefined ->
      {ok, Pid} = tyranny_zone_sup:start_child(ZoneId),
      Pid;
    Pid -> Pid
  end.

%% gen_server callbacks
init([ZoneData]) ->
  Zones = [create_zone(X) || X <- ZoneData],
  tyranny_zone_sup:start_link(Zones),
  {ok, #state{ zones = Zones}}.


handle_call({locate, Coord}, _From, #state{zones = Zones} = State) ->
  Zone = case [Z || Z <- Zones, coord_in_zone(Z, Coord)] of
           [] -> unknown;
           Z -> Z
         end,
  {reply, Zone, State};

handle_call(zones, _From, #state{zones = Zones} = State) ->
  {reply, Zones, State};

handle_call(_Request, _From, State) -> {reply, ignored, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal functions
create_zone({Id, {X, Y, Z}, {W, H}}) ->
  #zone{
    id = Id,
    top_left = {X, Y, Z},
    top_right = {X+W, Y, Z},
    bottom_left = {X, Y, Z+H},
    bottom_right = {X+W, Y, Z+H}
  }.

coord_in_zone(#zone{top_left = {X1,_,Z1}, top_right = {X2,_,_}, bottom_left = {_,_,Z2}}=_Zone, {X, _Y, Z} = _Coord) ->
  if
    X < X1 -> false;
    X > X2 -> false;
    Z < Z1 -> false;
    Z > Z2 -> false;
    true -> true
  end.
