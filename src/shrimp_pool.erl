-module(shrimp_pool).

-behaviour(gen_server).

-export([start_link/3, stop/1]).
-export([release/2, acquire/1]).
-export([notify_up/2, notify_down/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MAX_ID, 1000000).

start_link(Host, Port, #{max := _Max} = PoolSpec) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port, PoolSpec], []).

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
                  {ok, ConnPid} = shrimp_connection_sup:add_connection(Id, self(), Host, Port),
                  monitor(process, ConnPid),
                  Id
              end, [rand:uniform(?MAX_ID) || _ <- lists:seq(1, Max)]),
  {ok, #{pool_spec => PoolSpec, 
         conn_ids => ConnIds,
         waiting => [],
         available => []}}. 

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(acquire, From, #{available := [], waiting:=Waiting} = State) ->
  {noreply, State#{waiting => lists:append(Waiting, [From])}};

handle_call({release, ConnPid}, _From, #{waiting:=[Waiter | Waiters]} = State) ->
  gen_server:reply(Waiter, ConnPid),
  {reply, ok, State#{waiting => Waiters}};

handle_call({release, ConnPid}, _From, #{available := AvailableConns, waiting:=[]} = State) ->
  {reply, ok, State#{available => add_unique(ConnPid, AvailableConns)}};

handle_call(Request, From, State) ->
  logger:info("unhandled request ~p from ~p", [Request, From]),
  {reply, ok, State}. 

handle_cast({up, ConnPid}, #{waiting := [Waiter | Waiters]} = State) ->
  gen_server:reply(Waiter, ConnPid),
  {noreply, State#{waiting => Waiters}};

handle_cast({up, ConnPid}, #{available := AvailableConns, waiting:=[]} = State) ->
  {noreply, State#{available => add_unique(ConnPid, AvailableConns)}};

handle_cast({down, ConnPid}, #{available := AvailableConns} = State) ->
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

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

add_unique(A, L) -> 
  case lists:member(A, L) of
    true -> L;
    _ -> [A | L]
  end.

