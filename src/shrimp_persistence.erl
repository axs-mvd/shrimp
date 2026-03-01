-module(shrimp_persistence).

-export([load_config/0, save_config/1]).

%% Load configuration from JSON file specified by environment variable
%% SHRIMP_CONFIG_FILE defaults to "./config.json"
load_config() ->
  ConfigFile = get_config_file(),
  case file:read_file(ConfigFile) of
    {ok, Binary} ->
      try
        Json = jsx:decode(Binary, [return_maps, binary]),
        {ok, Json}
      catch
        _:_ -> {error, invalid_json}
      end;
    {error, enoent} ->
      {ok, #{backends => [], rules => []}};
    {error, Reason} ->
      {error, Reason}
  end.

%% Save configuration to JSON file
save_config(#{backends := Backends, rules := Rules}) ->
  ConfigFile = get_config_file(),
  BackendsList = maps:fold(fun(_Name, Backend, Acc) ->
                             [maps:without([pid], Backend) | Acc]
                           end, [], Backends),
  RulesList = maps:values(Rules),
  Config = #{
    backends => BackendsList,
    rules => RulesList
  },
  Json = jsx:encode(Config),
  case file:write_file(ConfigFile, Json) of
    ok -> ok;
    {error, Reason} ->
      logger:error("Failed to save config: ~p", [Reason]),
      ok
  end.

%% Get config file path from environment variable or use default
get_config_file() ->
  os:getenv("SHRIMP_CONFIG_FILE", "./config.json").
