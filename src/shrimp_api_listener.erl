-module(shrimp_api_listener).
-export([start/0]).

start() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/api/backend/[...]", shrimp_api_handler, []},
      {"/api/rule/[...]", shrimp_api_handler, []},
      {"/doc/[...]", shrimp_api_handler, []},
      {"/doc", shrimp_api_handler, []}
    ]}
  ]),
  R = {ok, _} = cowboy:start_clear(shrimp_api_listener,
                                   [{port, list_to_integer(os:getenv("SHRIMP_API_PORT", "8000"))}],
                                   #{env => #{dispatch => Dispatch}}),
  R.
