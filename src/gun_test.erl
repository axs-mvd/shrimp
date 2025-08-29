-module(gun_test).

-export([start/0]).
-export([test/0]).

start() ->
  application:ensure_all_started(gun).

test() ->
  {ok, ConnPid} = gun:open("example.org", 443),
  MonitorRef = monitor(process, ConnPid),
  StreamRef = gun:get(ConnPid, "/"),
  receive
    {gun_response, ConnPid, StreamRef, fin, Status, Headers} ->
      logger:info("Status: ~p", [Status]),
      logger:info("Headers: ~p", [Headers]),
      no_data;
    {gun_response, ConnPid, StreamRef, nofin, Status, Headers} ->
      logger:info("Status: ~p", [Status]),
      logger:info("Headers: ~p", [Headers]),
      receive_data(ConnPid, MonitorRef, StreamRef);
    {'DOWN', MonitorRef, process, ConnPid, Reason} ->
      error_logger:error_msg("Oops!"),
      exit(Reason)
  after 1000 ->
    exit(timeout)
  end.

receive_data(ConnPid, MonitorRef, StreamRef) ->
  receive
    {gun_data, ConnPid, StreamRef, nofin, Data} ->
      io:format("~s~n", [Data]),
      receive_data(ConnPid, MonitorRef, StreamRef);
    {gun_data, ConnPid, StreamRef, fin, Data} ->
      io:format("~s~n", [Data]);
    {'DOWN', MonitorRef, process, ConnPid, Reason} ->
      logger:error("Oops!"),
      exit(Reason)
  after 1000 ->
    logger:info("closing connection"),
    demonitor(MonitorRef),
    gun:close(ConnPid),
    exit(timeout)
  end.
