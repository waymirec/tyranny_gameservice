-module(grid_cell).
-behaviour(gen_server).
-author("waymirec").

%% API
-export([
  start_link/1,
  neighbors/1,
  objects/1,
  add_object/2,
  delete_object/2
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
  cell = #cell{},
  objects = []                :: [pid()]
}).

%% API functions
start_link(Cell) ->
  gen_server:start_link(?MODULE, [Cell], []).

neighbors(CellId) ->
  gen_server:call(CellId, neighbors).

objects(CellId) ->
  gen_server:call(CellId, objects).

add_object(CellId, Pid) when is_pid(Pid) ->
  gen_server:cast(CellId, {add_object, Pid}).

delete_object(CellId, Pid) when is_pid(Pid) ->
  gen_server:cast(CellId, {delete_object, Pid}).


%% gen_server callbacks
init([Cell]) ->
  {ok, #state{cell = Cell}, 0}.

handle_call(neighbors, _From, #state{cell = #cell{neighbors = Neighbors}} = State) ->
  {reply, {ok, Neighbors}, State};

handle_call(objects, _From, #state{objects = Objects} = State) ->
  {reply, {ok, Objects}, State};

handle_call(_Message, _From, State) ->
  {reply, reply, State}.

handle_cast({add_object, Pid}, #state{objects = Objects} = State) ->
  {noreply, State#state{objects = [Pid | filter(Pid, Objects)]}};

handle_cast({delete_object, Pid}, #state{objects = Objects} = State) ->
  {noreply, State#state{objects = filter(Pid, Objects)}};

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(timeout, #state{cell = #cell{id = CellId}} = State) ->
  register(CellId, self()),
  {noreply, State};

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal functions
filter(Pid, Users) ->
  lists:filter(fun(P) -> P =/= Pid end, Users).
