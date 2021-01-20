%%%-------------------------------------------------------------------
%%% @author waymirec
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jan 2021 10:03 PM
%%%-------------------------------------------------------------------
-module(tyranny_zone).
-author("waymirec").

-behavior(gen_server).

%% API callbacks
-export([
  start_link/1,
  objects/1,
  neighbors/1,
  add_object/2,
  delete_object/2,
  broadcast/4
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

-include("types.hrl").
-include("zone.hrl").

-record(state, {
  zone                  :: #zone{},
  objects = []          :: [pid()],
  neighbors = []        :: [pid()]
}).

-define(SERVER, ?MODULE).

%% API functions
start_link(ZoneId) ->
  gen_server:start_link(?MODULE, [ZoneId], []).

objects(ZoneId) ->
  gen_server:call(ZoneId, objects).

neighbors(ZoneId) ->
  gen_server:call(ZoneId, neighbors).

add_object(ZoneId, Pid) when is_pid(Pid) ->
  gen_server:cast(ZoneId, {add_object, Pid}).

delete_object(ZoneId, Pid) when is_pid(Pid) ->
  gen_server:cast(ZoneId, {delete_object, Pid}).

broadcast(ZoneId, {pid, Pid}, Message, Distance) ->
  gen_server:cast(ZoneId, {broadcast_from_pid, Pid, Message, Distance});

broadcast(ZoneId, {vector3, Vector}, Message, Distance) ->
  gen_server:cast(ZoneId, {broadcast_from_vector, Vector, Message, Distance}).

%% gen_server functions
init([ZoneId]) ->
  Zones = [create_zone(X) || X <- config:key(<<"zones">>)],
  [Zone | _Rest] = lists:filter(fun(Z) -> Z#zone.id =:= ZoneId end, Zones),
  State = #state{zone = Zone},
  {ok, State, 0}.

handle_call(objects, _From, State) ->
  {reply, State#state.objects, State}.

handle_cast({add_object, Pid}, State) ->
  {noreply, State#state{objects = [Pid | filter(Pid, State#state.objects)]}};

handle_cast({delete_object, Pid}, State) ->
  {noreply, State#state{objects = filter(Pid, State#state.objects)}};

handle_cast({broadcast_from_vector, Pid, Message, Distance}, State) ->
  Targets = lists:filter(fun(O) -> is_within_range(Pid, O, Distance) end, State#state.objects),
  lager:debug("Broadcasting to: ~w", [Targets]),
  lists:foreach(fun(O) -> gen_server:cast(O, {send_message, Message}) end, Targets),
  {noreply, State};

handle_cast({broadcast_from_pid, Origin, Message, Distance}, State) ->
  T1 = lists:filter(fun(O) -> is_within_range(Origin, O, Distance) end, State#state.objects),
  T2 = lists:filter(fun(O) -> O =/= Origin end, T1),
  lager:debug("Broadcasting to: ~w", [T2]),
  lists:foreach(fun(O) -> gen_server:cast(O, {send_message, Message}) end, T2),
  {noreply, State}.

handle_info(timeout, #state{zone = #zone{id = ZoneId}} = State) ->
  register(ZoneId, self()),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% internal functions

filter(Pid, Users) ->
  lists:filter(fun(P) -> P =/= Pid end, Users).

create_zone({Id, {X, Y, Z}, {W, H}, Neighbors}) ->
  #zone{
    id = Id,
    top_left = {X, Y, Z},
    top_right = {X+W, Y, Z},
    bottom_left = {X, Y, Z+H},
    bottom_right = {X+W, Y, Z+H},
    neighbors = Neighbors
  }.

is_within_range(OriginVector, Target, Range) when is_record(OriginVector, vector3) ->
  {ok, TargetVector} = gen_server:call(Target, location),
  vector3:distance(OriginVector, TargetVector) =< Range;

is_within_range(Origin, Target, Range) when is_pid(Origin) ->
  {ok, OriginVector} = gen_server:call(Origin, location),
  {ok, TargetVector} = gen_server:call(Target, location),
  vector3:distance(OriginVector, TargetVector) =< Range.
