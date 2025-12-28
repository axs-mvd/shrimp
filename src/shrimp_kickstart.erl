-module(shrimp_kickstart).

-export([init/2]).

-export([router/1]).
-export([do/1]).
-export([reply/1]).

init(Req0, State0) ->
  logger:info("~p", [Req0]),

  {ok, Body} = read_body(Req0),

  FinalState = lists:foldl(fun(Function, State)->
                               Function(State)
                            end, #{req => Req0, 
                                  body => Body}, 
                           funs()), 

  io:format("FinalState ~p~n", [FinalState]),

%  Req = cowboy_req:reply(200,
%                         #{<<"content-type">> => <<"text/plain">>},
%                          <<"Hello Pepe!">>,
%                         Req0),
  {ok, Req0, State0}.

funs() ->
  [
    fun shrimp_kickstart:router/1,
    fun shrimp_kickstart:do/1,
    fun shrimp_kickstart:reply/1
  ].

router(#{req := _Req} = Ctx) ->
  Pool = none,
  Ctx#{pool => Pool};

router(Ctx) -> 
  %@log here
  Ctx.

do(Ctx) ->
  Ctx.

reply(#{req := Req,
        reply := #{headers := Headers, 
                   body := Body,
                   status := Status}} = Ctx) ->
  cowboy_req:reply(Status, Headers, Body, Req),
  Ctx;

reply(#{req := Req} = Ctx) ->
  cowboy_req:reply(502, #{}, <<"Bad Gateway">>, Req),
  Ctx.

read_body(Req) ->
  read_body(cowboy_req:read_body(Req), <<>>).

read_body({more, Binary, Req}, CurrBinary) ->
  read_body(cowboy_req:read_body(Req), <<CurrBinary/binary, Binary/binary>>);

read_body({ok, Binary, _Req}, CurrBinary) ->
  {ok, <<CurrBinary/binary, Binary/binary>>}.

