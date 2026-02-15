-module(shrimp_model).

-behaviour(gen_server).

%% Public API
-export([
         start_link/0,
         stop/0,

         %% Backend operations
         add_backend/1,
         modify_backend/2,
         remove_backend/1,
         get_backend/1,
         list_backends/0,

         %% Rule operations
         add_rule/1,
         modify_rule/2,
         remove_rule/1,
         get_rule/1,
         list_rules/0,

         %% Callback management
         register_callback/2,
         unregister_callback/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type backend() :: #{name := string(),
                     url := string(),
                     pool_size := #{min := non_neg_integer(), max := non_neg_integer()}}.

-type rule() :: #{name := string(),
                  'in' := string(),
                  out := #{backends := [string()], dispatcher := atom()},
                  middlewares => list()}.

-type state() :: #{backends := #{string() => backend()},
                   rules := #{string() => rule()},
                   callbacks := #{atom() => [pid()]}}.

%% Public API

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:stop(?SERVER).

%% Backend Operations

-spec add_backend(backend()) -> {ok, string()} | {error, term()}.
add_backend(Backend) when is_map(Backend) ->
  gen_server:call(?SERVER, {add_backend, Backend}).

-spec modify_backend(string(), backend()) -> ok | {error, term()}.
modify_backend(Name, Backend) when is_list(Name), is_map(Backend) ->
  gen_server:call(?SERVER, {modify_backend, Name, Backend}).

-spec remove_backend(string()) -> ok | {error, term()}.
remove_backend(Name) when is_list(Name) ->
  gen_server:call(?SERVER, {remove_backend, Name}).

-spec get_backend(string()) -> {ok, backend()} | {error, not_found}.
get_backend(Name) when is_list(Name) ->
  gen_server:call(?SERVER, {get_backend, Name}).

-spec list_backends() -> {ok, [backend()]}.
list_backends() ->
  gen_server:call(?SERVER, list_backends).

%% Rule Operations

-spec add_rule(rule()) -> {ok, string()} | {error, term()}.
add_rule(Rule) when is_map(Rule) ->
  gen_server:call(?SERVER, {add_rule, Rule}).

-spec modify_rule(string(), rule()) -> ok | {error, term()}.
modify_rule(RuleId, Rule) when is_list(RuleId), is_map(Rule) ->
  gen_server:call(?SERVER, {modify_rule, RuleId, Rule}).

-spec remove_rule(string()) -> ok | {error, term()}.
remove_rule(RuleId) when is_list(RuleId) ->
  gen_server:call(?SERVER, {remove_rule, RuleId}).

-spec get_rule(string()) -> {ok, rule()} | {error, not_found}.
get_rule(RuleId) when is_list(RuleId) ->
  gen_server:call(?SERVER, {get_rule, RuleId}).

-spec list_rules() -> {ok, [rule()]}.
list_rules() ->
  gen_server:call(?SERVER, list_rules).

%% Callback Management

-spec register_callback(atom(), pid()) -> ok.
register_callback(Event, Pid) when is_atom(Event), is_pid(Pid) ->
  gen_server:call(?SERVER, {register_callback, Event, Pid}).

-spec unregister_callback(atom(), pid()) -> ok.
unregister_callback(Event, Pid) when is_atom(Event), is_pid(Pid) ->
  gen_server:call(?SERVER, {unregister_callback, Event, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  State = #{backends => #{},
            rules => #{},
            callbacks => #{}},
  {ok, State}.

handle_call({add_backend, Backend}, _From, State = #{backends := Backends}) ->
  case validate_backend(Backend) of
    {ok, Name} ->
      case maps:is_key(Name, Backends) of
        true ->
          {reply, {error, backend_already_exists}, State};
        false ->
          NewState = State#{backends := maps:put(Name, Backend, Backends)},
          notify_callbacks(on_backend_added, {Name, Backend}, State),
          {reply, {ok, Name}, NewState}
      end;
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;

handle_call({modify_backend, Name, Backend}, _From, State = #{backends := Backends}) ->
  case validate_backend(Backend) of
    {ok, ValidatedName} ->
      case ValidatedName =:= Name of
        false ->
          {reply, {error, backend_name_mismatch}, State};
        true ->
          case maps:is_key(Name, Backends) of
            false ->
              {reply, {error, backend_not_found}, State};
            true ->
              NewState = State#{backends := maps:put(Name, Backend, Backends)},
              notify_callbacks(on_backend_modified, {Name, Backend}, State),
              {reply, ok, NewState}
          end
      end;
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;

handle_call({remove_backend, Name}, _From, State = #{backends := Backends, rules := Rules}) ->
  case maps:is_key(Name, Backends) of
    false ->
      {reply, {error, backend_not_found}, State};
    true ->
      case backend_in_use(Name, Rules) of
        true ->
          {reply, {error, backend_in_use}, State};
        false ->
          Backend = maps:get(Name, Backends),
          NewState = State#{backends := maps:remove(Name, Backends)},
          notify_callbacks(on_backend_removed, {Name, Backend}, State),
          {reply, ok, NewState}
      end
  end;

handle_call({get_backend, Name}, _From, State = #{backends := Backends}) ->
  case maps:find(Name, Backends) of
    error ->
      {reply, {error, not_found}, State};
    {ok, Backend} ->
      {reply, {ok, Backend}, State}
  end;

handle_call(list_backends, _From, State = #{backends := Backends}) ->
  {reply, {ok, maps:values(Backends)}, State};

%% Rule operations
handle_call({add_rule, Rule}, _From, State = #{rules := Rules} = FullState) ->
  case validate_rule(Rule, FullState) of
    {ok, RuleName} ->
      case maps:is_key(RuleName, Rules) of
        true ->
          {reply, {error, rule_already_exists}, State};
        false ->
          NewState = State#{rules := maps:put(RuleName, Rule, Rules)},
          notify_callbacks(on_rule_added, {RuleName, Rule}, State),
          {reply, {ok, RuleName}, NewState}
      end;
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;

handle_call({modify_rule, RuleName, Rule}, _From, State = #{rules := Rules} = FullState) ->
  case validate_rule(Rule, FullState) of
    {ok, ValidatedName} ->
      case ValidatedName =:= RuleName of
        false ->
          {reply, {error, rule_name_mismatch}, State};
        true ->
          case maps:is_key(RuleName, Rules) of
            false ->
              {reply, {error, rule_not_found}, State};
            true ->
              NewState = State#{rules := maps:put(RuleName, Rule, Rules)},
              notify_callbacks(on_rule_modified, {RuleName, Rule}, State),
              {reply, ok, NewState}
          end
      end;
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;

handle_call({remove_rule, RuleName}, _From, State = #{rules := Rules}) ->
  case maps:is_key(RuleName, Rules) of
    false ->
      {reply, {error, rule_not_found}, State};
    true ->
      Rule = maps:get(RuleName, Rules),
      NewState = State#{rules := maps:remove(RuleName, Rules)},
      notify_callbacks(on_rule_removed, {RuleName, Rule}, State),
      {reply, ok, NewState}
  end;

handle_call({get_rule, RuleName}, _From, State = #{rules := Rules}) ->
  case maps:find(RuleName, Rules) of
    error ->
      {reply, {error, not_found}, State};
    {ok, Rule} ->
      {reply, {ok, Rule}, State}
  end;

handle_call(list_rules, _From, State = #{rules := Rules}) ->
  {reply, {ok, maps:values(Rules)}, State};

%% Callback management
handle_call({register_callback, Event, Pid}, _From, State = #{callbacks := Callbacks}) ->
  EventPids = maps:get(Event, Callbacks, []),
  case lists:member(Pid, EventPids) of
    true ->
      {reply, ok, State};
    false ->
      NewCallbacks = maps:put(Event, [Pid | EventPids], Callbacks),
      NewState = State#{callbacks := NewCallbacks},
      {reply, ok, NewState}
  end;

handle_call({unregister_callback, Event, Pid}, _From, State = #{callbacks := Callbacks}) ->
  EventPids = maps:get(Event, Callbacks, []),
  NewCallbacks = maps:put(Event, lists:delete(Pid, EventPids), Callbacks),
  NewState = State#{callbacks := NewCallbacks},
  {reply, ok, NewState};

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal helpers
%%%===================================================================

-spec validate_backend(map()) -> {ok, string()} | {error, term()}.
validate_backend(Backend) ->
  case Backend of
    #{name := Name, url := _URL, pool_size := PoolSize} when is_list(Name) ->
      case validate_pool_size(PoolSize) of
        ok -> {ok, Name};
        Error -> Error
      end;
    #{name := _Name, url := _URL, pool_size := _PoolSize} ->
      {error, invalid_name};
    #{name := _Name} ->
      {error, missing_url};
    #{url := _URL} ->
      {error, missing_name};
    #{pool_size := _PoolSize} ->
      {error, missing_name};
    _ ->
      {error, missing_name}
  end.

-spec validate_pool_size(term()) -> ok | {error, term()}.
validate_pool_size(#{min := Min, max := Max}) when is_integer(Min), is_integer(Max), Min >= 0, Max >= 0, Min =< Max ->
  ok;
validate_pool_size(_) ->
  {error, invalid_pool_size}.

-spec validate_rule(map(), state()) -> {ok, string()} | {error, term()}.
validate_rule(Rule, State) ->
  case Rule of
    #{name := RuleName} when not is_list(RuleName) ->
      {error, invalid_rule_name};
    #{name := RuleName, 'in' := _InPath, out := Out} when is_list(RuleName) ->
      case validate_out(Out, State) of
        ok -> {ok, RuleName};
        Error -> Error
      end;
    #{name := _RuleName, 'in' := _InPath} ->
      {error, missing_out};
    #{name := _RuleName, out := _Out} ->
      {error, missing_in_path};
    #{name := _RuleName} ->
      {error, missing_in_path};
    _ ->
      {error, missing_rule_name}
  end.

-spec validate_out(term(), state()) -> ok | {error, term()}.
validate_out(#{backends := BackendNames, dispatcher := Dispatcher}, State) when is_list(BackendNames), is_atom(Dispatcher), length(BackendNames) > 0 ->
  check_backends_exist(BackendNames, State);
validate_out(#{backends := _}, _State) ->
  {error, missing_dispatcher};
validate_out(#{dispatcher := _}, _State) ->
  {error, missing_backends_in_out};
validate_out(_, _State) ->
  {error, missing_out}.

-spec check_backends_exist([string()], state()) -> ok | {error, term()}.
check_backends_exist([], _State) ->
  ok;
check_backends_exist([Name | Rest], State = #{backends := Backends}) ->
  case maps:is_key(Name, Backends) of
    false ->
      {error, {backend_not_found, Name}};
    true ->
      check_backends_exist(Rest, State)
  end.

-spec backend_in_use(string(), #{string() => rule()}) -> boolean().
backend_in_use(BackendName, Rules) ->
  maps:fold(fun(_RuleId, #{out := #{backends := Backends}}, Acc) ->
                Acc orelse lists:member(BackendName, Backends)
            end, false, Rules).

-spec notify_callbacks(atom(), term(), state()) -> ok.
notify_callbacks(Event, Data, #{callbacks := Callbacks}) ->
  EventPids = maps:get(Event, Callbacks, []),
  lists:foreach(fun(Pid) ->
                    Pid ! {?MODULE, Event, Data}
                end, EventPids),
  ok.
