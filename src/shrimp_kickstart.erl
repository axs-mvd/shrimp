-module(shrimp_kickstart).

-export([init/2]).

-export([router/1]).
-export([do/1]).
-export([reply/1]).

init(Req0, State0) ->
  logger:info("~p", [Req0]),
  logger:info("~p", [State0]),

  {ok, Body} = read_body(Req0),

  FinalState = lists:foldl(fun(Function, State)->
                               Function(State)
                            end, #{req => Req0, 
                                   body => Body}, 
                           funs()), 

  logger:info("FinalState ~p", [FinalState]),

  {ok, Req0, State0}.

funs() -> [fun shrimp_kickstart:router/1,
           fun shrimp_kickstart:do/1,
           fun shrimp_kickstart:reply/1].

router(#{req := Req} = Ctx) ->
  {ok, PoolPid} = shrimp_router:route(Req),
  Ctx#{pool => PoolPid};

router(Ctx) -> 
  %@log here
  Ctx.

do(#{pool := PoolPid,
     req := Req,
     body := Body} = Ctx) ->
  {ok, Conn} = shrimp_pool:acquire(PoolPid),
  logger:info("acquired a connection ~p", [Conn]),
  {ok, Resp} = shrimp_connection:request(Conn, 
                                   #{method => cowboy_req:method(Req), 
                                     url => cowboy_req:uri(Req),
                                     body => Body,
                                     headers => change_headers(cowboy_req:headers(Req))}),
  shrimp_pool:release(PoolPid, Conn),
  Ctx#{reply => Resp}.

%change_headers(#{<<"host">> := Host} = OriginalHeaders) ->
%  maps:without([<<"host">>], 
%               OriginalHeaders#{<<"X-Forwarded-For">> => Host,
%                                <<"Host">> => <<"example.org">>}).

change_headers(#{<<"host">> := Host} = OriginalHeaders) ->
  OriginalHeaders#{<<"X-Forwarded-For">> => Host,
                   <<"Host">> => <<"example.org">>}.


reply(#{req := Req,
        reply := #{headers := Headers, 
                   data := Body,
                   status := Status}} = Ctx) ->
  cowboy_req:reply(Status, lists:foldl(fun({Key, Value}, M) -> maps:put(Key, Value, M)
                                       end, #{}, Headers), Body, Req),
  Ctx;

reply(#{req := Req} = Ctx) ->
  logger:error("Didn't find a suitable response for that ~p", [Ctx]),
  cowboy_req:reply(502, #{}, <<"Bad Gateway, urraca">>, Req),
  Ctx.

read_body(Req) ->
  read_body(cowboy_req:read_body(Req), <<>>).

read_body({more, Binary, Req}, CurrBinary) ->
  read_body(cowboy_req:read_body(Req), <<CurrBinary/binary, Binary/binary>>);

read_body({ok, Binary, _Req}, CurrBinary) ->
  {ok, <<CurrBinary/binary, Binary/binary>>}.

