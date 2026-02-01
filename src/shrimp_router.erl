-module(shrimp_router).

-behaviour(gen_server).

-export([start/0]).
-export([route/1]).


-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([terminate/2]).
-export([code_change/3]).


start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

route(Req) ->
  gen_server:call(?MODULE, {route, Req}).

init([]) ->
  {ok, PoolPid} = shrimp_pool:start_link("localhost", 8000, #{max => 3}), 
  logger:info("connected to example.org crash test dummy"),
  {ok, #{pool => PoolPid}}.

handle_call({route, Req}, _From, #{pool := PoolPid} = State) ->
  logger:info("routing ~p", [Req]),
  {reply, {ok, PoolPid}, State}.

handle_cast(_, _) ->
  error(not_implemented).

terminate(_, _) ->
  error(not_implemented).

code_change(_, _, _) ->
  error(not_implemented).


