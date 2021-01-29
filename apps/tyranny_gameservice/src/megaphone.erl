-module(megaphone).
-author("waymirec").

-include_lib("kernel/include/logger.hrl").
-include("vector3.hrl").
-include("grid.hrl").

%% API
-export([
  broadcast/3,
  discover_objects/2
]).

broadcast(Message, SourcePid, Range) when is_binary(Message) and is_pid(SourcePid) and is_float(Range) ->
  Objects = discover_objects(SourcePid, Range),
  io:format("Objects: ~w~n", [Objects]),
  send_message(Message, Objects),
  %send_message(Message, discover_objects(SourcePid, Range)),
  ok;

broadcast(Message, PointOfOrigin, Range) when is_binary(Message) and is_record(PointOfOrigin, vector3) and is_float(Range) ->
  send_message(Message, discover_objects(PointOfOrigin, Range)),
  ok.

discover_objects(SourcePid, Range) when is_pid(SourcePid) and is_float(Range) ->
  {ok, PointOfOrigin} = player:location(SourcePid),
  #cell{id = CellId} = locate_cell(PointOfOrigin),
  AllObjects = lists:filter(fun(Pid) -> Pid =/= SourcePid end, retrieve_cell_objects(CellId)),
  filter_objects_within_range(PointOfOrigin, AllObjects, Range);

discover_objects(PointOfOrigin, Range) when is_record(PointOfOrigin, vector3) and is_float(Range) ->
  #cell{id = CellId} = locate_cell(PointOfOrigin),
  AllObjects = retrieve_cell_objects(CellId),
  filter_objects_within_range(PointOfOrigin, AllObjects, Range).

retrieve_cell_objects(CellId) ->
  Objects = get_cell_objects(CellId),
  Neighbors = get_cell_neighbors(CellId),
  lists:flatten(retrieve_cell_objects(CellId, Neighbors, Objects)).

retrieve_cell_objects(CellId, [Neighbor | Neighbors], Objects) ->
  [get_cell_objects(Neighbor) | retrieve_cell_objects(CellId, Neighbors,Objects)];

retrieve_cell_objects(_CellId, [], Objects) ->
  Objects.

locate_cell(Pid) when is_pid(Pid) ->
  {ok, Vector} = player:location(Pid),
  locate_cell(Vector);

locate_cell(Vector) when is_record(Vector, vector3) ->
  {ok, Cell} = grid_manager:locate(Vector),
  Cell.

get_cell_objects(CellId) ->
  {ok, Objects} = grid_cell:objects(CellId),
  Objects.

get_cell_neighbors(CellId) ->
  {ok, Neighbors} = grid_cell:neighbors(CellId),
  Neighbors.

filter_objects_within_range(Origin, Objects, Range) ->
  Filter = fun(Pid) -> is_within_range(Origin, Pid, Range) end,
  lists:filter(Filter, Objects).

is_within_range(OriginVector, Target, Range) when is_record(OriginVector, vector3) ->
  {ok, TargetVector} = player:location(Target),
  vector3:distance(OriginVector, TargetVector) =< Range;

is_within_range(Origin, Target, Range) when is_pid(Origin) ->
  {ok, OriginVector} = gen_server:call(Origin, location),
  {ok, TargetVector} = gen_server:call(Target, location),
  vector3:distance(OriginVector, TargetVector) =< Range.


send_message(Message, [Pid | Pids]) when is_pid(Pid) ->
  gen_server:cast(Pid, {send_message, Message}),
  send_message(Message, Pids);

send_message(_Message, []) ->
  ok.