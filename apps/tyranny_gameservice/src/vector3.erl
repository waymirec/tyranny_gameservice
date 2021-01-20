-module(vector3).

-include("vector3.hrl").

%% API
-export([distance/2]).

distance(#vector3{x=X1,y=Y1,z=Z1}, #vector3{x=X2,y=Y2,z=Z2}) ->
  math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2) + math:pow(Z1 - Z2, 2)).
