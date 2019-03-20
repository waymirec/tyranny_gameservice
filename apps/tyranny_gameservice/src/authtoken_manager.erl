-module(authtoken_manager).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([validate/2]).

-record(state, {
  db
}).

validate(UserName, TokenValue) ->
  gen_server:call(?MODULE, {validate, UserName, TokenValue}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  DbHost = config:key(<<"authtoken_db.host">>),
  DbPort = config:key(<<"authtoken_db.port">>),
  DbDatabase = config:key(<<"authtoken_db.database">>),
  DbPassword = config:key(<<"authtoken_db.password">>),
  ConnectTimeout = config:key(<<"authtoken_db.connect_timeout">>),
  ReconnectDelay = config:key(<<"authtoken_db.reconnect_delay">>),
  {ok, DbClient} = eredis:start_link(DbHost, DbPort, DbDatabase, DbPassword, ReconnectDelay, ConnectTimeout),
  State = #state{db = DbClient},
  {ok, State}.

handle_call({validate, UserName, TokenValue}, _From, State) ->
  #state{db = DbClient} = State,
  Result = do_validate_token(UserName, TokenValue, DbClient),
  {reply, Result, State};

handle_call(_Request, _From, State) -> {reply, ignored, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal functions

do_validate_token(UserName, TokenValue, DbClient) ->
  case eredis:q(DbClient, ["EXISTS", TokenValue]) of
    {ok, <<"0">>} -> false;
    {ok, <<"1">>} ->
      {ok, TokenData} = eredis:q(DbClient, ["GET", TokenValue]),
      do_validate_token_data(UserName, TokenData)
  end.

do_validate_token_data(UserName, TokenData) ->
  <<ForUserNameLength:8, ForUserName:ForUserNameLength/binary, ForServerIpBin:32>> = TokenData,
  ForServerIp = inet_util:int_to_ip(ForServerIpBin),
  ThisServerIp = config:key(<<"listener.vip">>),
  NameMatches = UserName =:= ForUserName,
  ServerIpMatches = ThisServerIp =:= ForServerIp,
  case {NameMatches, ServerIpMatches} of
    {true, true} -> true;
    {_, _} -> false
  end.
