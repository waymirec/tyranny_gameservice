-module(uuid).

-export([
  create/0,
  create/1,
  to_list/1
]).

create() -> crypto:strong_rand_bytes(16).

create(Str) when is_list(Str) ->
  hex:hexstr_to_bin(Str).

to_list(Bin) when is_binary(Bin) ->
  hex:bin_to_hexstr(Bin).
