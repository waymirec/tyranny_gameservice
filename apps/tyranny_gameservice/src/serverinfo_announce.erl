-module(serverinfo_announce).
-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([ send_hello/3 ]).
-record(state, {
        socket					:: term(),
        hello_tref				:: term()
       }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Port = config:hello_port(),
    Addr = config:hello_addr(), 
    Interval = config:hello_interval(),
    {ok, Socket} = gen_udp:open(Port, [{reuseaddr,true}, {ip,Addr}, {multicast_ttl,4}, {multicast_loop,true}, binary]),
    inet:setopts(Socket,[{add_membership,{Addr,{0,0,0,0}}}, {active,once}]),
    {ok, TRef} = timer:apply_interval(Interval, ?MODULE, send_hello, [Socket, Addr, Port]),

    State = #state{socket=Socket, hello_tref=TRef},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    #state{hello_tref=TRef} = State,
    timer:cancel(TRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

send_hello(Socket, Address, Port) ->
    lager:debug("Sending Hello Packet.", []),
    Packet = build_hello_packet(),
    ok = gen_udp:send(Socket, Address, Port, Packet),
    ok.

build_hello_packet() ->
    Name = list_to_bitstring(net_adm:localhost()),
    NameLength = bit_size(Name),
    IntIp = inet_util:ip_to_int({127,0,0,1}),
    IntPort = 12346,
    ExtIp = inet_util:ip_to_int({127,0,0,2}),
    ExtPort = 54321,
    NumConns = 5,
    SysLoadLast = 4,
    SysLoad1 = 3,
    SysLoad5 = 2,
    SysLoad15 = 1,
    MemTotal = 321,
    MemFree = 123,

    << NameLength:8, 
       Name:NameLength/bitstring, 
       IntIp:32,
       IntPort:32,
       ExtIp:32, 
       ExtPort:32, 
       NumConns:16, 
       SysLoadLast:8, 
       SysLoad1:8, 
       SysLoad5:8, 
       SysLoad15:8, 
       MemTotal:32, 
       MemFree:32
    >>.
