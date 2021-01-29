-module(game_object).
-author("waymirec").

%% API
-export([
  location/1
]).


location(Pid) -> gen_server:call(Pid, {?MODULE, location}).
