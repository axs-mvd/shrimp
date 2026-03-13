-module(shrimp_router).

-behaviour(gen_server).

-export([start/0]).
-export([route/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([terminate/2]).
-export([code_change/3]).

%% only for test purposes
-export([pick_backend/1, pick_rule/1]).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

route(Req) ->
  gen_server:call(?MODULE, {route, Req}).

init([]) ->
  {ok, #{}}.

handle_call({route, Req}, _From, State) ->
  logger:info("routing ~p", [Req]),
    {reply, pick_backend(pick_rule(Req)), State}.

handle_cast(_, _) ->
  error(not_implemented).

terminate(_, _) ->
  ok.

code_change(_, State, _) ->
  {ok, State}.


pick_rule(Req) ->
  Path = cowboy_req:path(Req),
  {ok, Rules} = shrimp_model:list_rules(),

  case lists:dropwhile(fun(Rule) ->
                           not match(Path, Rule)
                       end, Rules) of
    [] -> no_match;
    [TheOne | _] -> TheOne
  end.

match(Path, #{'in' := In}) ->
  K = size(In),
  case Path of
    <<In:K/binary, _/binary>> -> true;
    _ -> false 
  end.

strategy(#{out := #{dispatcher := Strategy}}) when is_atom(Strategy)->
  Strategy.

is_backend_alive(#{pid := _}) -> true.

pick_backend(no_match) -> {error, no_match};

pick_backend(#{out := #{backends := BackendNames}} = Rule) ->
  Backends = lists:map(fun(BackendName) -> 
                           {ok, Backend} = shrimp_model:get_backend(to_binary(BackendName)), 
                           Backend 
                       end, BackendNames),
  case lists:dropwhile(fun(Backend) -> 
                           not is_backend_alive(Backend)
                       end, sort_backends(strategy(Rule), Backends)) of
    [] -> {error, no_backend_available};
    [#{pid := Pid}| _] -> {ok, Pid}
  end.

to_binary(B) when is_binary(B) -> B;
to_binary(B) when is_list(B) -> list_to_binary(B).

%to_atom(B) when is_atom(B) -> B;
%to_atom(B) when is_binary(B) -> binary_to_atom(B);
%to_atom(B) when is_list(B) -> list_to_atom(B).

sort_backends(random, Backends) -> 
  lists:sort(fun(_, _) -> rand:uniform() > 0.5 end, Backends);

sort_backends(first_alive, Backends) -> Backends;

sort_backends(Strategy, Backends) -> 
  logger:error("illegal strategy ~p while choosing from ~p", [Strategy, Backends]),
  {error, illegal_strategy}.

