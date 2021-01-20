-module(game_event).
-behaviour(gen_server).
-author("waymirec").

%% API
-export([
  start_link/0,
  add_handler/2,
  delete_handler/2,
  delete_handler/1,
  send_event/2,
  game_object_spawned/1,
  game_object_destroyed/1
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

-record(state, {
  handlers = #{}               :: map()
}).

%% API callbacks

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

add_handler(Pid, Tags) when (is_atom(Pid) or is_pid(Pid)) and is_list(Tags) ->
  gen_server:call(?SERVER, {add_handler, Pid, Tags});

add_handler(Pid, Tag) when (is_atom(Pid) or is_pid(Pid)) and is_atom(Tag) ->
  gen_server:call(?SERVER, {add_handler, Pid, [Tag]}).

delete_handler(Pid, Tags) when is_pid(Pid) and is_list(Tags) ->
  gen_server:call(?SERVER, {delete_handler, Pid, Tags});

delete_handler(Pid, Tag) when is_pid(Pid) and is_atom(Tag) ->
  gen_server:call(?SERVER, {delete_handler, Pid, [Tag]}).

delete_handler(Pid) when is_pid(Pid) ->
  gen_server:call(?SERVER, {delete_handler, Pid}).

game_object_spawned(Pid) when is_pid(Pid) ->
  gen_server:cast(?SERVER, {game_object_spawned, Pid}).

game_object_destroyed(Pid) when is_pid(Pid) ->
  gen_server:cast(?SERVER, {game_object_destroyed, Pid}).

send_event(Tag, Message) ->
  gen_server:cast(?SERVER, {Tag, Message}).

%% gen_server callbacks

init([]) ->
  {ok, #state{}}.

handle_cast({Tag, _} = Message, #state{handlers = Handlers} = State) when is_atom(Tag) ->
  do_send_event(Message, Handlers),
  {noreply, State};

handle_cast(_Message, State) ->
  {noreply, State}.

handle_call({add_handler, Pid, Tags}, _From, #state{handlers = Handlers} = State) when is_list(Tags) ->
  Handlers2 = do_add_handlers(Pid, Tags, Handlers),
  {reply, ok, State#state{handlers = Handlers2}};

handle_call({delete_handler, Pid, Tags}, _From, #state{handlers = Handlers} = State) when is_list(Tags) ->
  Handlers2 = do_delete_handlers(Pid, Tags, Handlers),
  {reply, ok, State#state{handlers = Handlers2}};

handle_call({delete_handler, Pid}, _From, #state{handlers = Handlers} = State) ->
  Handlers2 = do_delete_handlers(Pid, Handlers),
  {reply, ok, State#state{handlers = Handlers2}}.

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal functions

do_add_handlers(_Pid, [], Handlers) ->
  Handlers;

do_add_handlers(Pid, [Tag | Tags], Handlers) ->
  Handlers2 = do_delete_handlers(Pid, [Tag], Handlers),

  Handlers3 = maps:put(Tag, [Pid | maps:get(Tag, Handlers2, [])], Handlers2),
  do_add_handlers(Pid, Tags, Handlers3).

do_delete_handlers(Pid, Handlers) ->
  Keys = maps:keys(maps:filter(fun(_K,V) -> lists:member(Pid, V) end, Handlers)),
  lists:foreach(fun(K) -> maps:update(K, lists:filter(fun(P) -> P =/= Pid end, maps:get(K, Handlers)), Handlers) end, Keys).

do_delete_handlers(_Pid, [], Handlers) ->
  Handlers;

do_delete_handlers(Pid, [Tag | Tags], Handlers) ->
  List = maps:get(Tag, Handlers, []),
  Handlers2 = filter_handler(Tag, List, Handlers),
  do_delete_handlers(Pid, Tags, Handlers2).

do_send_event({Tag, _} = Message, Handlers) ->
  List = maps:get(Tag, Handlers, []),
  lists:foreach(fun(P) -> P ! Message end, List),
  ok.

filter_handler(_Tag, [], Handlers) ->
  Handlers;

filter_handler(Tag, [Pid | Rest], Handlers) ->
  Handlers2 = maps:update(Tag, lists:filter(fun(P) -> P =/= Pid end, maps:get(Tag, Handlers)), Handlers),
  filter_handler(Tag, Rest, Handlers2).