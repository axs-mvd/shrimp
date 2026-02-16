-module(shrimp_api_listener).
-export([start/0]).

start() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/backend/[...]", shrimp_api_handler, []},
      {"/rule/[...]", shrimp_api_handler, []},
      {"/doc/[...]", shrimp_api_handler, []},
      {"/doc", shrimp_api_handler, []}
    ]}
  ]),
  R = {ok, _} = cowboy:start_clear(shrimp_api_listener,
                                   [{port, 8000}],
                                   #{env => #{dispatch => Dispatch}}),
  R.
