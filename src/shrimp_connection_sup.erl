-module(shrimp_connection_sup).

-behaviour(supervisor).
-export([start_link/0, init/1]).
-export([add_connection/3]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  ChildSpecs = [], % Initially no children
  {ok, {SupFlags, ChildSpecs}}.

add_connection(Id, Host, Port) ->
  ChildSpec = #{id => Id,
                start => {shrimp_connection, start_link, [Host, Port]},
                type => worker,
                restart => permanent,
                shutdown => 5000},
  supervisor:start_child(?MODULE, ChildSpec).

