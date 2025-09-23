-module(shrimp_pool).

-behaviour(gen_server).

-export([start_link/3, request/2, stop/1, notify_available/2, notify_death/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MAX_ID, 1000000).

start_link(Host, Port, #{max := _Max} = PoolSpec) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port, PoolSpec], []).

stop(ServerRef) ->
  gen_server:call(ServerRef, stop).

request(ServerRef, Request) ->
  gen_server:call(ServerRef, {request, Request}).

notify_available(ServerRef, ConnPid) ->
  gen_server:cast(ServerRef, {available, ConnPid}).

notify_death(ServerRef, ConnPid) ->
  gen_server:cast(ServerRef, {death, ConnPid}).

init([Host, Port, #{max := Max} = PoolSpec]) ->
  ConnIds = lists:map(
              fun(Id) -> 
                  {ok, ConnPid} = shrimp_connection_sup:add_connection(Id, self(), Host, Port),
                  monitor(process, ConnPid),
                  Id
              end, [rand:uniform(?MAX_ID) || _ <- lists:seq(1, Max)]),
  {ok, #{pool_spec => PoolSpec, 
         conn_ids => ConnIds,
         available => []}}. 

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(Request, From, State) ->
  logger:info("unhandled request ~p from ~p", [Request, From]),
  {reply, ok, State}. % Default for unhandled calls

handle_cast(Msg, State) ->
  logger:info("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info(Info, State) ->
  logger:info("unhandled info ~p", [Info]),
  {noreply, State}. 

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

