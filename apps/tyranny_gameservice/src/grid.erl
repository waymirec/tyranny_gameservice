-module(grid).

%% API
-export([
  create_grid/1
]).

-include("vector3.hrl").
-include("grid.hrl").

create_grid(#grid_def{width = W, height = H, cell_height = CH, cell_width = CW} = GridDef) ->
  NumCols = trunc(math:ceil(W / CW)),
  NumRows = trunc(math:ceil(H / CH)),
  GridDef2 = GridDef#grid_def{columns = NumCols, rows = NumRows},
  [ create_cell({R, C}, GridDef2) || R <- lists:seq(0, NumRows-1), C <- lists:seq(0, NumCols-1)].

create_cell({Row, Column}, #grid_def{cell_width = CellWidth, cell_height = CellHeight, columns = NumCols} = GridDef) ->
  CellNumber = ((Row*NumCols)+1 * Column+1),
  X = (CellWidth * Column) / 1,
  Z = (CellHeight * Row) / 1,
  Y = 0.0,

  #cell{
    id = create_cell_id(CellNumber),
    top_left = #vector3{x = X, y = Y, z = Z},
    top_right = #vector3{x = X + CellWidth, y = Y, z = Z},
    bottom_left = #vector3{x = X, y = Y, z = Z + CellHeight},
    bottom_right = #vector3{x = X + CellWidth, y = Y, z = Z + CellHeight},
    neighbors = neighbors(GridDef, CellNumber)
  }.

neighbors(GridDef, CellNumber) -> neighbors(GridDef, CellNumber, []).

%% left edge
neighbors(#grid_def{columns = NumCols} = GridDef, CellNumber, Neighbors) when ((CellNumber =:= 1) or ((CellNumber -1) rem NumCols =:= 0)) ->
  N1 = east(GridDef, CellNumber, Neighbors),
  N2 = southeast(GridDef, CellNumber, N1),
  N3 = south(GridDef, CellNumber, N2),
  N4 = north(GridDef, CellNumber, N3),
  N5 = northeast(GridDef, CellNumber, N4),
  N5;

%% right edge
neighbors(#grid_def{columns = NumCols} = GridDef, CellNumber, Neighbors) when (CellNumber rem NumCols =:= 0) ->
  N1 = south(GridDef, CellNumber, Neighbors),
  N2 = southwest(GridDef, CellNumber, N1),
  N3 = west(GridDef, CellNumber, N2),
  N4 = northwest(GridDef, CellNumber, N3),
  N5 = north(GridDef, CellNumber, N4),
  N5;

neighbors(GridDef, CellNumber, Neighbors) ->
  N1 = east(GridDef, CellNumber, Neighbors),
  N2 = southeast(GridDef, CellNumber, N1),
  N3 = south(GridDef, CellNumber, N2),
  N4 = southwest(GridDef, CellNumber, N3),
  N5 = west(GridDef, CellNumber, N4),
  N6 = northwest(GridDef, CellNumber, N5),
  N7 = north(GridDef, CellNumber, N6),
  N8 = northeast(GridDef, CellNumber, N7),
  N8.

east(_GridDef, CellNumber, Neighbors) when CellNumber > 0 -> add_neighbor(CellNumber + 1, Neighbors).
southeast(#grid_def{columns = NumCols}, CellNumber, Neighbors) -> add_neighbor(CellNumber + (NumCols+1), Neighbors).
south(#grid_def{columns = NumCols}, CellNumber, Neighbors) -> add_neighbor(CellNumber + NumCols, Neighbors).
southwest(#grid_def{columns = NumCols}, CellNumber, Neighbors) -> add_neighbor(CellNumber + (NumCols-1), Neighbors).
west(_GridDef, CellNumber, Neighbors) -> add_neighbor(CellNumber - 1, Neighbors).
northwest(#grid_def{columns = NumCols}, CellNumber, Neighbors) -> add_neighbor(CellNumber - (NumCols+1), Neighbors).
north(#grid_def{columns = NumCols}, CellNumber, Neighbors) -> add_neighbor(CellNumber - NumCols, Neighbors).
northeast(#grid_def{columns = NumCols}, CellNumber, Neighbors) -> add_neighbor(CellNumber - (NumCols-1), Neighbors).

add_neighbor(CellNumber, Neighbors) when CellNumber > 0 ->
  [create_cell_id(CellNumber) | Neighbors];

add_neighbor(_CellNumber, Neighbors) ->
  Neighbors.

create_cell_id(Number) ->
  list_to_atom(lists:flatten(io_lib:format("cell~s", [integer_to_list(Number)]))).