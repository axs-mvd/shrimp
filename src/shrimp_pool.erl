-module(shrimp_pool).

-behaviour(gen_server).

-export([start_link/3, start_link/4, stop/1]).
-export([release/2, acquire/1]).
-export([notify_up/2, notify_down/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MAX_ID, 1000000).

start_link(Id, Host, Port, #{max := _Max} = PoolSpec) ->
  gen_server:start_link({local, Id}, ?MODULE, [Host, Port, PoolSpec], []).

start_link(Host, Port, #{max := _Max} = PoolSpec) ->
  gen_server:start_link(?MODULE, [Host, Port, PoolSpec], []).

stop(ServerRef) ->
  gen_server:call(ServerRef, stop).

release(ServerRef, ConnPid) ->
  gen_server:call(ServerRef, {release, ConnPid}).

acquire(ServerRef) ->
  gen_server:call(ServerRef, acquire).

notify_up(ServerRef, ConnPid) ->
  gen_server:cast(ServerRef, {up, ConnPid}).

notify_down(ServerRef, ConnPid) ->
  gen_server:cast(ServerRef, {down, ConnPid}).

init([Host, Port, #{max := Max} = PoolSpec]) ->
  ConnIds = lists:map(
              fun(Id) -> 
                  case shrimp_connection_sup:add_connection(Id, self(), Host, Port) of
                    {ok, ConnPid} -> monitor(process, ConnPid);
                    Other -> logger:warning("error ~p", [Other]) 
                  end,
                  Id
              end, [rand:uniform(?MAX_ID) || _ <- lists:seq(1, Max)]),
  {ok, #{pool_spec => PoolSpec, 
         conn_ids => ConnIds,
         waiting => [],
         available => []}}. 

handle_call(stop, _From, #{conn_ids := ConnIds} = State) ->
  lists:foreach(fun(ConnId) -> 
                    shrimp_connection_sup:remove_connection(ConnId) 
                end, ConnIds),
  {stop, normal, ok, State};

handle_call(acquire, _From, #{available := [ConnPid | ConnPids]} = State) ->
  {reply, {ok, ConnPid}, State#{available => ConnPids}};

handle_call(acquire, From, #{available := [], waiting := Waiting} = State) ->
  {noreply, State#{waiting => lists:append(Waiting, [From])}};

handle_call({release, ConnPid}, _From, #{waiting := [Waiter | Waiters]} = State) ->
  gen_server:reply(Waiter, ConnPid),
  {reply, ok, State#{waiting => Waiters}};

handle_call({release, ConnPid}, _From, #{available := AvailableConns, waiting := []} = State) ->
  {reply, ok, State#{available => add_unique(ConnPid, AvailableConns)}};

handle_call(Request, From, State) ->
  logger:info("unhandled request ~p from ~p", [Request, From]),
  {reply, ok, State}. 

handle_cast({up, ConnPid}, #{waiting := [Waiter | Waiters]} = State) ->
  gen_server:reply(Waiter, ConnPid),
  {noreply, State#{waiting => Waiters}};

handle_cast({up, ConnPid}, #{available := AvailableConns, waiting := []} = State) ->
  logger:debug("we have a new connection ~p we have ATM ~p open conns", [ConnPid, length(AvailableConns)]),
  {noreply, State#{available => add_unique(ConnPid, AvailableConns)}};

handle_cast({down, ConnPid}, #{available := AvailableConns} = State) ->
  logger:warning("hate to say we lost a connection ~p we have currently ~p open conns", [ConnPid, length(AvailableConns)]),
  {noreply, State#{available => lists:remove(ConnPid, AvailableConns)}};

handle_cast(Request, State) ->
  logger:info("unhandled cast ~p", [Request]),
  {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, ConnPid, Reason}, #{available := AvailableConns} = State) ->
  logger:warning("Connection down ~p reason: ~p", [ConnPid, Reason]),
  {noreply, State#{available => lists:remove(ConnPid, AvailableConns)}};

handle_info(Info, State) ->
  logger:info("unhandled info ~p", [Info]),
  {noreply, State}. 

terminate(Reason, #{available := AvailableConns, waiting := Waiters} = State) ->
  logger:info("replying to waiters.."),
  lists:foreach(fun(Waiter) -> 
                    gen_server:reply(Waiter, {error, terminate}) 
                end, Waiters),
  logger:info("closing all conns"),
  lists:foreach(fun(Conn) -> 
                    shrimp_connection_sup:remove_connection(Conn) 
                end, AvailableConns),
  logger:info("terminate ~p ~p", [Reason, State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

add_unique(A, L) -> 
  case lists:member(A, L) of
    true -> L;
    _ -> [A | L]
  end.

