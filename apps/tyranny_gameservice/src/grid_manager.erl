-module(grid_manager).
-behaviour(gen_server).
-author("waymirec").

%% API
-export([
  start_link/0,
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

-include("vector3.hrl").
-include("grid.hrl").

-record(state, {
  grid = []                       :: [#cell{}]
}).

%% API callbacks
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

locate(Vector) when is_record(Vector, vector3) ->
  gen_server:call(?SERVER, {locate, Vector}).

%% gen_server callbacks
init([]) ->
  Grid = grid:create_grid(create_grid_def(config:key(<<"grid">>))),
  {ok, #state{grid = Grid}, 0}.

handle_call({locate, Vector}, _From, #state{grid = Grid} = State) ->
  {reply, which_cell(Vector, Grid), State};

handle_call(_Message, _From, State) ->
  {ok, reply, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(timeout, #state{grid = Grid} = State) ->
  grid_sup:start_link(),
  start_cell_processes(Grid),
  game_event:add_handler(self(), [game_object_spawned, game_object_destroyed]),
  {noreply, State};

%% Event callbacks
handle_info({game_object_spawned, {Pid, Vector}}, #state{grid = Grid} = State) ->
  track_object(Pid, Vector, Grid),
  {noreply, State};

handle_info({game_object_destroyed, Pid}, #state{grid = Grid} = State) ->
  clear_object_tracking(Pid, Grid),
  {noreply, State};

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  game_event:delete_handler(self(), []),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% internal functions
create_grid_def(RawGridDef) ->
  #grid_def{
    width = get_config_value(width, RawGridDef),
    height = get_config_value(height, RawGridDef),
    cell_height = get_config_value(cell_height, RawGridDef),
    cell_width = get_config_value(cell_width, RawGridDef)
  }.

get_config_value(Param, Data) ->
  {Param, Value} = proplists:lookup(Param, Data),
  Value.

start_cell_processes([]) ->
  ok;

start_cell_processes([#cell{id = Id} = Cell| Rest]) ->
  case whereis(Id) of
    undefined ->
      {ok, _Pid} = grid_sup:start_child(Cell);
    _Pid -> ok
  end,
  start_cell_processes(Rest).

which_cell(Vector, Grid) ->
  case [Cell || Cell <- Grid, coord_in_cell(Cell, Vector)] of
    [] -> unknown;
    [Cell] -> {ok, Cell}
  end.

coord_in_cell(#cell{top_left = #vector3{x=X1,z=Z1}, top_right = #vector3{x=X2}, bottom_left = #vector3{z=Z2}}=_Cell, #vector3{x=X,z=Z}) ->
  if
    X < X1 -> false;
    X > X2 -> false;
    Z < Z1 -> false;
    Z > Z2 -> false;
    true -> true
  end.

track_object(Pid, Vector, Grid) ->
  lists:foreach(fun(#cell{id = CellId}) -> grid_cell:delete_object(CellId, Pid) end, Grid),
  {ok, #cell{id = CellId}} = which_cell(Vector, Grid),
  grid_cell:add_object(CellId, Pid).

clear_object_tracking(Pid, Grid) ->
  lists:foreach(fun(#cell{id = CellId}) -> grid_cell:delete_object(CellId, Pid) end, Grid).
