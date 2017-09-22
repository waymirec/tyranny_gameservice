-module(authservice_handler).
-behavior(ranch_protocol).
-behavior(gen_server).

%% Ranch Callbacks
-export([ start_link/4,
	  init/1
   	]).

%% Gen_Server Callbacks
-export([ handle_cast/2,
	  handle_call/3,
	  handle_info/2,
	  terminate/2,
	  code_change/3
	]).

-export([ ping/2 ]).

-record(state, {
        ref                                     :: ranch:ref(),
        socket                                  :: gen_tcp:socket(), 
        transport                               :: module(),
	hello_tref				:: any(),
	ping_interval				:: integer(),
	ping_timeout				:: integer()
       }).

-type state() :: #state{}.

-spec start_link(Ref :: ranch:ref(), Socket :: gen_udp:socket(), Transport :: module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
    %% start the process syncronously to avoid deadlock
    {ok, proc_lib:spawn_link(?MODULE, init, [[Ref, Socket, Transport, Opts]])}.

-spec init(Args :: list()) -> no_return().
init([Ref, Socket, Transport, _Opts = []]) ->
    %% Perform any required state initialization here
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, 4}]),

    PingInterval = config:key(<<"authservice.listener.ping_interval">>),
    PingTimeout = config:key(<<"authservice.listener.ping_timeout">>),
    {ok, TRef} = timer:apply_interval(PingInterval, ?MODULE, ping, [Socket, Transport]),
    State = #state{ref=Ref, socket=Socket, transport=Transport, hello_tref=TRef,
		   ping_interval=PingInterval, ping_timeout=PingTimeout},
    gen_server:enter_loop(?MODULE, [], State, PingTimeout).

-spec handle_info(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_info({tcp, Socket, <<1:32>>}, State) ->
    #state{socket=Socket, transport=Transport, ping_timeout=Timeout} = State,
    Transport:setopts(Socket, [{active, once}]),
    lager:debug("Pong!", []),
    {noreply, State, Timeout};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};

handle_info(timeout, State) ->
    lager:debug("Got Timeout!", []), 
    {stop, normal, State};

handle_info(Info, State) ->
    lager:debug("Unexpected info: ~p", [Info]),
    {noreply, State}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()}, State :: state()) -> Result :: {reply, Reply :: term(), NewState :: state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: state()) -> ok.
terminate(_Reason, State) ->
    #state{hello_tref=TRef, socket=Socket} = State,
    lager:debug("Terminating...", []),
    timer:cancel(TRef), 
    gen_tcp:close(Socket),
    ok.

-spec code_change(OldVsn :: {down, term()} | term(), State :: gen_server:state(), Extra :: term()) -> {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

ping(Socket, Transport) ->
    lager:debug("Ping!", []),
    Transport:send(Socket, <<0:32>>),
    ok.
