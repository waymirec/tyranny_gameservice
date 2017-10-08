-module(gameservice_publisher).
-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, { 
        server_name,
        db, 
        timer,
	ttl
       }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    DbHost = config:key(<<"gameserver_db.host">>),
    DbPort = config:key(<<"gameserver_db.port">>),
    DbDatabase = config:key(<<"gameserver.db_database">>),
    DbPassword = config:key(<<"gameserver_db.password">>),
    ConnectTimeout = config:key(<<"gameserver_db.connect_timeout">>),
    ReconnectDelay = config:key(<<"gameserver_db.reconnect_delay">>),
    UpdateInterval = config:key(<<"gameserver_db.update_interval">>),
    TTL = config:key(<<"gameserver_db.ttl">>),
    {ok, DbClient} = eredis:start_link(DbHost, DbPort, DbDatabase, DbPassword, ReconnectDelay, ConnectTimeout),
    {ok, TRef} = timer:send_interval(UpdateInterval, self(), update_info),
    ServerName = net_adm:localhost(),
    State = #state{server_name=ServerName, db=DbClient, timer=TRef, ttl=TTL},
    {ok, State}.

handle_info(update_info, State) ->
    update_info(State),
    {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ignored, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, State) -> 
    #state{timer=Timer} = State,
    timer:cancel(Timer), 
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal functions
update_info(State) ->
    #state{server_name=ServerName, db=DbClient, ttl=TTL} = State,
    Info = build_info(),
    {ok, <<"OK">>} = eredis:q(DbClient, ["SET", ServerName, Info]),
    {ok, _} = eredis:q(DbClient, ["EXPIRE", ServerName, TTL]),
    ok.

build_info() ->
    Name = list_to_bitstring(net_adm:localhost()),
    NameLength = bit_size(Name),
    Ip = inet_util:ip_to_int(config:key(<<"listener.vip">>)),
    Port = config:key(<<"listener.port">>),
    Score = calculate_score(),

    << NameLength:8, 
       Name:NameLength/bitstring, 
       Ip:32, 
       Port:32, 
       Score:16
    >>.

calculate_score() ->
%   Util = cpu_sup:util(),
%   PercentLoad = 100 * (1 - 50/(50 + Util)),
%   MemoryData = memsup:get_system_memory_data(),    
%   FreeMem = proplists:get_value(free_memory, MemoryData),
%   Score = FreeMem / PercentLoad,
    Score = rand:uniform(100),
    Score.


