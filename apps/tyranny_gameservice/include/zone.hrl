
-include("vector3.hrl").

-record(zone, {
  id              :: atom(),
  top_left        :: vector3(),
  top_right       :: vector3(),
  bottom_left     :: vector3(),
  bottom_right    :: vector3(),
  neighbors = []  :: [atom()]
}).