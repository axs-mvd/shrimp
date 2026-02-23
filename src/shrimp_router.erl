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
  {ok, #{}}.

handle_call({route, Req}, _From, State) ->
  logger:info("routing ~p", [Req]),
  {ok, Rules} = shrimp_model:list_rules(),
  {reply, pick_backend(route(Req, Rules)), State}.

handle_cast(_, _) ->
  error(not_implemented).

terminate(_, _) ->
  error(not_implemented).

code_change(_, _, _) ->
  error(not_implemented).

route(_, []) ->
  no_match;

route(Req, [#{name := Name,
              backends := _,
              'in' := In} = Rule | Rules]) ->
  case match(cowboy_req:path(Req), Rule) of
    match -> 
      logger:info("match on ~s url: ~p in rule ~p", [Name, cowboy_req:path(Req), In]),
      Rule;
    no_match ->
      logger:info("no match on ~s url: ~p in rule ~p", [Name, cowboy_req:path(Req), In]),
      route(Req, Rules)
  end.

match(Path, #{'in' := In}) ->
  K = size(In),
  case Path of
    <<In:K/binary, _/binary>> -> match;
    _ -> no_match 
  end.

strategy(#{dispatcher := _}) ->
  random.

pick_backend(Rule) ->
  pick_backend(strategy(Rule), Rule).

pick_backend(random, #{backends := Backends}) -> 
  #{pid := PoolPid} = lists:nth(rand:uniform(length(Backends)), Backends),
  {ok, PoolPid};

pick_backend(Strategy, Backends) -> 
  logger:error("illegal strategy ~p while choosing from ~p", [Strategy, Backends]),
  none.

