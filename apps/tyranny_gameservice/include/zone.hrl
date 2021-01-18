
-include("types.hrl").

-record(zone, {
  id            :: atom(),
  top_left      :: coord(),
  top_right     :: coord(),
  bottom_left   :: coord(),
  bottom_right  :: coord()
}).