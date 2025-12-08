-module(shrimp_connection_sup).

-behaviour(supervisor).
-export([start_link/0, init/1]).
-export([add_connection/4]).
-export([remove_connection/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  ChildSpecs = [], % Initially no children
  {ok, {SupFlags, ChildSpecs}}.

add_connection(Id, PoolPid, Host, Port) ->
  ChildSpec = #{id => Id,
                start => {shrimp_connection, start_link, [PoolPid, Host, Port]},
                type => worker,
                restart => permanent,
                shutdown => 5000},
  supervisor:start_child(?MODULE, ChildSpec).

remove_connection(Id) ->
  supervisor:terminate_child(?MODULE, Id),
  supervisor:delete_child(?MODULE, Id).

