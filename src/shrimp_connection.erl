-module(shrimp_connection).

-behaviour(gen_statem).

%% API
-export([start_link/2, stop/1, event/2]).
%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([command/3, receiving/3]).
-export([get/2]).
-export([start/2]).

-define(SERVER, ?MODULE).

start(Host, Port) -> start_link(Host, Port).

start_link(Host, Port) ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [Host, Port], []).

stop(Pid) ->
  gen_statem:stop(Pid).

event(Pid, Event) ->
  gen_statem:cast(Pid, Event).

get(Pid, Url) ->
  gen_statem:call(Pid, {request, #{method => get, 
                                   url => Url, 
                                   header => []}}).

callback_mode() ->
  state_functions.  %% using state_functions style

init([Host, Port]) ->
  io:format("Starting state machine in idle~n"),
  {ok, ConnPid} = gun:open(Host, Port),
  MonitorRef = monitor(process, ConnPid),
  {ok,
   command,
   #{host => Host,
     port => Port,
     monitor_ref => MonitorRef,
     conn => ConnPid}}.   %% initial state is idle with empty map as data

command({call, CallerPid},
        {request,
         #{method := post,
           url := Url,
           body := Body,
           header := Headers}},
        #{conn := ConnPid} = State) ->
  _StreamRef = gun:post(ConnPid, Url, Headers, Body),
  {next_state, receiving, State#{caller => CallerPid}};

command({call, CallerPid},
        {request,
         #{method := get,
           url := Url,
           header := Headers}},
        #{conn := ConnPid} = State) ->
  _StreamRef = gun:get(ConnPid, Url, Headers),
  {next_state, receiving, State#{caller => CallerPid}};

command(info, Msg, Data) ->
  io:format("Got raw message in command: ~p~n", [Msg]),
  {keep_state, Data};
command(info, 
        {'DOWN', MonitorRef, process, ConnPid, Reason}, 
        #{conn := ConnPid, monitor_ref:= MonitorRef}) ->
  error_logger:error_msg("Oops!"),
  exit(Reason).

receiving(cast, Req, State) ->
  io:format("Got a cast on receiving: ~p~n", [Req]),
  {next_state, receiving, State};
receiving(info, 
          {gun_response, ConnPid, _StreamRef, nofin, Status, Headers}, 
          #{conn := ConnPid} = State) ->
  logger:info("Status: ~p", [Status]),
  logger:info("Headers: ~p", [Headers]),
  {next_state, receiving, State#{response => #{status => Status, 
                                               headers => Headers, 
                                               data => empty}}};
receiving(info, 
          {gun_response, ConnPid, _StreamRef, fin, Status, Headers}, 
          #{conn := ConnPid, 
            caller := CallerPid} = State) ->
  logger:info("Status: ~p", [Status]),
  logger:info("Headers: ~p", [Headers]),
  ok = gen_statem:reply(CallerPid, {ok, #{status => Status, headers => Headers}}),
  {next_state, command, State};
receiving(info, 
          {gun_data, ConnPid, _StreamRef, nofin, Data}, 
          #{conn := ConnPid, 
            response := #{data := PrevData} = Response} = State) ->
  PartialData = add(PrevData, Data),
  logger:info("PartialData: ~p", [PartialData]),
  {next_state, receiving, State#{response => Response#{data => PartialData}}};
receiving(info, 
          {gun_data, ConnPid, _StreamRef, fin, Data}, 
          #{conn := ConnPid, 
            caller := CallerPid, 
            response := #{data := PrevData} = Response} = State) ->
  CompletedData = add(PrevData, Data),
  logger:info("CompletedData: ~p", [CompletedData]),
  gen_statem:reply(CallerPid, {ok, Response#{data => CompletedData}}),
  {next_state, command, maps:without([response], State)};
receiving(info, 
          {'DOWN', MonitorRef, process, ConnPid, Reason}, 
          #{conn := ConnPid, monitor_ref := MonitorRef}) ->
  error_logger:error_msg("Oops!"),
  exit(Reason).

add(empty, D) -> D;
add(A,B) -> <<A/binary, B/binary>>.

code_change(_OldVsn, State, Data, _Extra) -> 
  {ok, State, Data}.

terminate(_Reason, _State, _Data) ->
  logger:info("~p terminating.", [self()]),
  ok.
