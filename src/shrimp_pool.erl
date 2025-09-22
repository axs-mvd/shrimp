-module(shrimp_pool).

-behaviour(gen_server).

-export([start_link/0, increment/0, get_count/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment() ->
    gen_server:cast(?MODULE, increment).

get_count() ->
    gen_server:call(?MODULE, get_count).

stop() ->
    gen_server:call(?MODULE, stop).

init(_Args) ->
    {ok, 0}. % Initial state is 0

handle_call(get_count, _From, State) ->
    {reply, State, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}. % Default for unhandled calls

handle_cast(increment, State) ->
    NewState = State + 1,
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}. % Default for unhandled casts

handle_info(_Info, State) ->
    {noreply, State}. % Default for unhandled info messages

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

