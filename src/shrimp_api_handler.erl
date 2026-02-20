-module(shrimp_api_handler).

-export([init/2]).

init(Req0, State) ->
  Method = cowboy_req:method(Req0),
  Path = cowboy_req:path(Req0),
  Req = route_request(Method, Path, Req0),
  {ok, Req, State}.

route_request(Method, Path, Req0) ->
  case check_path(Path) of
    backend ->
      handle_backend(Method, Path, Req0);
    rule ->
      handle_rule(Method, Path, Req0);
    doc when Method =:= <<"GET">> ->
      serve_swagger_yaml(Req0);
    _ ->
      cowboy_req:reply(404, Req0)
  end.

check_path(Path) ->
  case binary:match(Path, <<"/api/backend">>) of
    nomatch ->
      case binary:match(Path, <<"/api/rule">>) of
        nomatch ->
          case Path of
            <<"/doc">> -> doc;
            _ -> unknown
          end;
        _ -> rule
      end;
    _ -> backend
  end.

%% Swagger endpoint
serve_swagger_yaml(Req) ->
  case read_swagger_file() of
    {ok, Content} ->
      cowboy_req:reply(200, #{<<"content-type">> => <<"application/yaml">>}, Content, Req);
    {error, _Reason} ->
      cowboy_req:reply(500, Req)
  end.

read_swagger_file() ->
  PrivDir = code:priv_dir(shrimp),
  FilePath = filename:join(PrivDir, "swagger.yaml"),
  file:read_file(FilePath).

%% Backend endpoints
handle_backend(Method, Path, Req0) ->
  PathList = binary:split(Path, <<"/">>, [global]),
  Name = case PathList of
    [_, <<"api">>, <<"backend">>] -> undefined;
    [_, <<"api">>, <<"backend">>, NameBin] -> NameBin;
    _ -> undefined
  end,
  handle_backend_request(Method, Name, Req0).

handle_backend_request(<<"GET">>, undefined, Req) ->
  handle_list_backends(Req);
handle_backend_request(<<"GET">>, Name, Req) ->
  handle_get_backend(Req, Name);
handle_backend_request(<<"POST">>, undefined, Req) ->
  handle_create_backend(Req);
handle_backend_request(<<"PUT">>, Name, Req) ->
  handle_update_backend(Req, Name);
handle_backend_request(<<"DELETE">>, Name, Req) ->
  handle_delete_backend(Req, Name);
handle_backend_request(_, _, Req) ->
  cowboy_req:reply(405, Req).

handle_list_backends(Req) ->
  case shrimp_model:list_backends() of
    {ok, Backends} ->
      CleanBackends = [sanitize_backend(B) || B <- Backends],
      shrimp_api_utils:reply_json(Req, 200, CleanBackends);
    {error, Reason} ->
      Status = shrimp_api_utils:error_to_status(Reason),
      shrimp_api_utils:reply_error(Req, Status, Reason)
  end.

handle_get_backend(Req, Name) ->
  case shrimp_model:get_backend(binary_to_list(Name)) of
    {ok, Backend} ->
      CleanBackend = sanitize_backend(Backend),
      shrimp_api_utils:reply_json(Req, 200, CleanBackend);
    {error, not_found} ->
      shrimp_api_utils:reply_error(Req, 404, not_found);
    {error, Reason} ->
      Status = shrimp_api_utils:error_to_status(Reason),
      shrimp_api_utils:reply_error(Req, Status, Reason)
  end.

handle_create_backend(Req0) ->
  {ok, Body, Req1} = cowboy_req:read_body(Req0),
  %@TODO instead of only decode the body, create a validator of the incoming data
  case decode_json(Body) of
    {ok, Data} ->
      Backend = prepare_backend(Data),
      case shrimp_model:add_backend(Backend) of
        {ok, _Name} ->
          shrimp_api_utils:reply_json(Req1, 201, #{<<"status">> => <<"created">>});
        {error, backend_already_exists} ->
          shrimp_api_utils:reply_json(Req1, 200, #{<<"status">> => <<"ok">>});
        {error, Reason} ->
          Status = shrimp_api_utils:error_to_status(Reason),
          shrimp_api_utils:reply_error(Req1, Status, Reason)
      end;
    {error, _} ->
      shrimp_api_utils:reply_error(Req1, 400, invalid_json)
  end.

handle_update_backend(Req0, Name) ->
  {ok, Body, Req1} = cowboy_req:read_body(Req0),
  %@TODO instead of only decode the body, create a validator of the incoming data
  case decode_json(Body) of
    {ok, Data} ->
      Backend = prepare_backend(Data),
      case shrimp_model:modify_backend(binary_to_list(Name), Backend) of
        ok ->
          shrimp_api_utils:reply_json(Req1, 200, #{<<"status">> => <<"ok">>});
        {error, backend_not_found} ->
          shrimp_api_utils:reply_error(Req1, 404, backend_not_found);
        {error, Reason} ->
          Status = shrimp_api_utils:error_to_status(Reason),
          shrimp_api_utils:reply_error(Req1, Status, Reason)
      end;
    {error, _} ->
      shrimp_api_utils:reply_error(Req1, 400, invalid_json)
  end.

handle_delete_backend(Req0, Name) ->
  case shrimp_model:remove_backend(binary_to_list(Name)) of
    ok ->
      shrimp_api_utils:reply_json(Req0, 200, #{<<"status">> => <<"ok">>});
    {error, backend_not_found} ->
      shrimp_api_utils:reply_error(Req0, 404, backend_not_found);
    {error, Reason} ->
      Status = shrimp_api_utils:error_to_status(Reason),
      shrimp_api_utils:reply_error(Req0, Status, Reason)
  end.

prepare_backend(Data) ->
  PoolSizeData = maps:get(<<"pool-size">>, Data, #{}),
  PoolSize = #{
    min => maps:get(<<"min">>, PoolSizeData, 1),
    max => maps:get(<<"max">>, PoolSizeData, 10)
  },
  #{
    name => binary_to_list(maps:get(<<"name">>, Data, <<>>)),
    url => binary_to_list(maps:get(<<"url">>, Data, <<>>)),
    pool_size => PoolSize
  }.

%% Rule endpoints
handle_rule(Method, Path, Req0) ->
  PathList = binary:split(Path, <<"/">>, [global]),
  Name = case PathList of
    [_, <<"api">>, <<"rule">>] -> undefined;
    [_, <<"api">>, <<"rule">>, NameBin] -> NameBin;
    _ -> undefined
  end,
  handle_rule_request(Method, Name, Req0).

handle_rule_request(<<"GET">>, undefined, Req) ->
  handle_list_rules(Req);
handle_rule_request(<<"GET">>, Name, Req) ->
  handle_get_rule(Req, Name);
handle_rule_request(<<"POST">>, undefined, Req) ->
  handle_create_rule(Req);
handle_rule_request(<<"PUT">>, Name, Req) ->
  handle_update_rule(Req, Name);
handle_rule_request(<<"DELETE">>, Name, Req) ->
  handle_delete_rule(Req, Name);
handle_rule_request(_, _, Req) ->
  cowboy_req:reply(405, Req).

handle_list_rules(Req) ->
  case shrimp_model:list_rules() of
    {ok, Rules} ->
      CleanRules = [sanitize_rule(R) || R <- Rules],
      shrimp_api_utils:reply_json(Req, 200, CleanRules);
    {error, Reason} ->
      Status = shrimp_api_utils:error_to_status(Reason),
      shrimp_api_utils:reply_error(Req, Status, Reason)
  end.

handle_get_rule(Req, Name) ->
  case shrimp_model:get_rule(binary_to_list(Name)) of
    {ok, Rule} ->
      CleanRule = sanitize_rule(Rule),
      shrimp_api_utils:reply_json(Req, 200, CleanRule);
    {error, not_found} ->
      shrimp_api_utils:reply_error(Req, 404, not_found);
    {error, Reason} ->
      Status = shrimp_api_utils:error_to_status(Reason),
      shrimp_api_utils:reply_error(Req, Status, Reason)
  end.

handle_create_rule(Req0) ->
  {ok, Body, Req1} = cowboy_req:read_body(Req0),
  case decode_json(Body) of
    {ok, Data} ->
      Rule = prepare_rule(Data),
      case shrimp_model:add_rule(Rule) of
        {ok, _Name} ->
          shrimp_api_utils:reply_json(Req1, 201, #{<<"status">> => <<"created">>});
        {error, rule_already_exists} ->
          shrimp_api_utils:reply_json(Req1, 200, #{<<"status">> => <<"ok">>});
        {error, Reason} ->
          Status = shrimp_api_utils:error_to_status(Reason),
          shrimp_api_utils:reply_error(Req1, Status, Reason)
      end;
    {error, _} ->
      shrimp_api_utils:reply_error(Req1, 400, invalid_json)
  end.

handle_update_rule(Req0, Name) ->
  {ok, Body, Req1} = cowboy_req:read_body(Req0),
  case decode_json(Body) of
    {ok, Data} ->
      Rule = prepare_rule(Data),
      case shrimp_model:modify_rule(binary_to_list(Name), Rule) of
        ok ->
          shrimp_api_utils:reply_json(Req1, 200, #{<<"status">> => <<"ok">>});
        {error, rule_not_found} ->
          shrimp_api_utils:reply_error(Req1, 404, rule_not_found);
        {error, Reason} ->
          Status = shrimp_api_utils:error_to_status(Reason),
          shrimp_api_utils:reply_error(Req1, Status, Reason)
      end;
    {error, _} ->
      shrimp_api_utils:reply_error(Req1, 400, invalid_json)
  end.

handle_delete_rule(Req0, Name) ->
  case shrimp_model:remove_rule(binary_to_list(Name)) of
    ok ->
      shrimp_api_utils:reply_json(Req0, 200, #{<<"status">> => <<"ok">>});
    {error, rule_not_found} ->
      shrimp_api_utils:reply_error(Req0, 404, rule_not_found);
    {error, Reason} ->
      Status = shrimp_api_utils:error_to_status(Reason),
      shrimp_api_utils:reply_error(Req0, Status, Reason)
  end.

prepare_rule(Data) ->
  #{
    name => binary_to_list(maps:get(<<"name">>, Data, <<>>)),
    'in' => binary_to_list(maps:get(<<"in">>, Data, <<>>)),
    out => prepare_out(maps:get(<<"out">>, Data, #{})),
    middlewares => maps:get(<<"middlewares">>, Data, [])
  }.

prepare_out(OutData) ->
  DispatcherBin = maps:get(<<"dispatcher">>, OutData, <<"round_robin">>),
  Dispatcher = case DispatcherBin of
                 <<"round_robin">> -> round_robin;
                 Atom when is_atom(Atom) -> Atom;
                 _ -> round_robin
               end,
  #{
    backends => [binary_to_list(B) || B <- maps:get(<<"backends">>, OutData, [])],
    dispatcher => Dispatcher
  }.

%% Utilities
decode_json(Body) ->
  try
    {ok, jsx:decode(Body, [return_maps])}
  catch
    _:_ -> {error, invalid_json}
  end.

%% Sanitize backends for JSON response (remove pid field and convert to binary keys)
sanitize_backend(Backend) ->
  BackendWithoutPid = maps:without([pid], Backend),
  PoolSize = maps:get(pool_size, BackendWithoutPid),
  #{
    <<"name">> => list_to_binary(maps:get(name, BackendWithoutPid)),
    <<"url">> => list_to_binary(maps:get(url, BackendWithoutPid)),
    <<"pool-size">> => #{
      <<"min">> => maps:get(min, PoolSize),
      <<"max">> => maps:get(max, PoolSize)
    }
  }.

%% Sanitize rules for JSON response (convert to binary keys)
sanitize_rule(Rule) ->
  OutData = maps:get(out, Rule),
  #{
    <<"name">> => list_to_binary(maps:get(name, Rule)),
    <<"in">> => list_to_binary(maps:get('in', Rule)),
    <<"out">> => #{
      <<"backends">> => [list_to_binary(B) || B <- maps:get(backends, OutData)],
      <<"dispatcher">> => atom_to_binary(maps:get(dispatcher, OutData), utf8)
    },
    <<"middlewares">> => maps:get(middlewares, Rule, [])
  }.
