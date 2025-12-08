-module(shrimp_listener).
-export([start/0]).


start() ->
  Dispatch = cowboy_router:compile([
                                    {'_', [{"/[...]", shrimp_kickstart, []}]}
                                   ]),
  R = {ok, _} = cowboy:start_clear(shrimp_http_listener,
                                   [{port, 8080}],
                                   #{env => #{dispatch => Dispatch}}),
  R.
