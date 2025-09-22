-module(shrimp_connection).

-behaviour(gen_statem).

-export([start_link/2, stop/1, event/2]).
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([command/3, receiving/3]).
-export([request/2]).
-export([get/2]).
-export([post/3]).
-export([post/4]).
-export([start/2]).

-define(SERVER, ?MODULE).

start(Host, Port) ->
  start_link(Host, Port).

start_link(Host, Port) ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [Host, Port], []).

stop(Pid) ->
  gen_statem:stop(Pid).

event(Pid, Event) ->
  gen_statem:cast(Pid, Event).

get(Pid, Url) ->
  gen_statem:call(Pid,
                  {request,
                   #{method => get,
                     url => Url,
                     headers => []}}).

post(Pid, Url, Body, Headers) ->
  gen_statem:call(Pid,
                  {request,
                   #{method => post,
                     url => Url,
                     body => Body,
                     headers => Headers}}).

post(Pid, Url, Body) ->
  post(Pid, Url, Body, []).

request(Pid, Req) ->
  gen_statem:call(Pid, {request, Req}).

callback_mode() ->
  state_functions.

init([Host, Port]) ->
  logger:log(info, "connecting to ~p:~p:", [Host, Port]),
  {ok, ConnPid} = gun:open(Host, Port),
  logger:log(info, "waiting for gun"),
  MonitorRef = monitor(process, ConnPid),
  {ok, command, #{host => Host,
                  port => Port,
                  monitor_ref => MonitorRef,
                  conn => ConnPid}}.
command({call, CallerPid},
        {request, #{method := post,
                    url := Url,
                    body := Body,
                    headers := Headers}},
        #{conn := ConnPid} = State) ->
  _StreamRef = gun:post(ConnPid, Url, Headers, Body),
  {next_state, receiving, State#{caller => CallerPid}};
command({call, CallerPid}, 
        {request, Request}, 
        #{conn := ConnPid} = State) ->
  _StreamRef = fire(ConnPid, Request),
  {next_state, receiving, State#{caller => CallerPid}};
command(info,
        {'DOWN', MonitorRef, process, ConnPid, Reason},
        #{conn := ConnPid,
          monitor_ref := MonitorRef,
          caller := CallerPid}) ->
  error_logger:error_msg(Reason),
  demonitor(MonitorRef, flush),
  gen_statem:reply(CallerPid, {error, {connection_down, Reason}}),
  exit(Reason);
command(info, Msg, Data) ->
  io:format("Got raw message in command: ~p~n", [Msg]),
  {keep_state, Data}.

fire(ConnPid,
     #{method := Method,
       url := Path,
       headers := Headers,
       body := Body} = Req) ->
  logger:log(info, "calling gun ~p", [Req]),
  gun:request(ConnPid, Method, Path, Headers, Body);
fire(ConnPid,
     #{method := _Method,
       url := _Path,
       body := _Body} = Req) ->
  fire(ConnPid, Req#{headers => []});
fire(ConnPid,
     #{method := _Method,
       headers := _Headers,
       url := _Path} = Req) ->
  fire(ConnPid, Req#{body => <<"">>});
fire(ConnPid, #{method := _Method, url := _Path} = Req) ->
  fire(ConnPid, Req#{headers => [], body => <<"">>}).

receiving(cast, Req, State) ->
  logger:log(info, "Got a cast on receiving: ~p~n", [Req]),
  {next_state, receiving, State};
receiving(info,
          {gun_response, ConnPid, _StreamRef, nofin, Status, Headers},
          #{conn := ConnPid} = State) ->
  {next_state,
   receiving,
   State#{response =>
            #{status => Status,
              headers => Headers,
              data => empty}}};
receiving(info,
          {gun_response, ConnPid, _StreamRef, fin, Status, Headers},
          #{conn := ConnPid, caller := CallerPid} = State) ->
  ok = gen_statem:reply(CallerPid, {ok, #{status => Status, headers => Headers}}),
  {next_state, command, State};
receiving(info,
          {gun_data, ConnPid, _StreamRef, nofin, Data},
          #{conn := ConnPid, response := #{data := PrevData} = Response} = State) ->
  PartialData = add(PrevData, Data),
  {next_state, receiving, State#{response => Response#{data => PartialData}}};
receiving(info,
          {gun_data, ConnPid, _StreamRef, fin, Data},
          #{conn := ConnPid,
            caller := CallerPid,
            response := #{data := PrevData} = Response} =
            State) ->
  CompletedData = add(PrevData, Data),
  gen_statem:reply(CallerPid, {ok, Response#{data => CompletedData}}),
  {next_state, command, maps:without([response], State)};
receiving(info,
          {'DOWN', MonitorRef, process, ConnPid, Reason},
          #{conn := ConnPid,
            monitor_ref := MonitorRef,
            caller := CallerPid}) ->
  error_logger:error_msg(Reason),
  demonitor(MonitorRef, flush),
  gen_statem:reply(CallerPid, {error, {connection_down, Reason}}),
  exit(Reason);
receiving(info, Msg, Data) ->
  logger:log(info, "Got raw message in receiving: ~p~n", [Msg]),
  {keep_state, Data}.

add(empty, D) ->
  D;
add(A, B) ->
  <<A/binary, B/binary>>.

code_change(_OldVsn, State, Data, _Extra) ->
  {ok, State, Data}.

terminate(_Reason, _State, #{conn := ConnPid, monitor_ref := MonRef}) ->
  demonitor(MonRef),
  gun:close(ConnPid),
  logger:info("~p terminating.", [self()]),
  ok.
