-record(grid_def, {
  width             :: float(),
  height            :: float(),
  columns           :: float(),
  rows              :: float(),
  cell_width        :: float(),
  cell_height       :: float()
}).

-record(cell, {
  id              :: atom(),
  top_left        :: vector3(),
  top_right       :: vector3(),
  bottom_left     :: vector3(),
  bottom_right    :: vector3(),
  neighbors = []  :: [atom()]
}).