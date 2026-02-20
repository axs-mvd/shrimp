-module(shrimp_pool_sup).

-behaviour(supervisor).
-export([start_link/0, init/1]).
-export([add_pool/4]).
-export([remove_pool/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  ChildSpecs = [], % Initially no children
  {ok, {SupFlags, ChildSpecs}}.

add_pool(Id, Host, Port, PoolSpec) ->
  ChildSpec = #{id => Id,
                start => {shrimp_pool, start_link, [Host, Port, PoolSpec]},
                type => worker,
                restart => permanent,
                shutdown => 5000},
  supervisor:start_child(?MODULE, ChildSpec).

remove_pool(Id) ->
  supervisor:terminate_child(?MODULE, Id),
  supervisor:delete_child(?MODULE, Id).
