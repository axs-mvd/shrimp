-module(shrimp_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  shrimp_listener:start(),
  shrimp_connection_sup:start_link(),
  shrimp_router:start(),
	shrimp_sup:start_link().

stop(_State) ->
	ok.
