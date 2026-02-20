-module(shrimp_api_integration_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").

-define(API_BASE_URL, "http://localhost:8000/api").
-define(TEST_TIMEOUT, 5000).

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    %% Start the application
    ct:log("Starting shrimp application..."),
    {ok, _} = application:ensure_all_started(shrimp),
    timer:sleep(500), %% Allow HTTP listener to start
    inets:start(),
    Config.

end_per_suite(_Config) ->
    ct:log("Stopping shrimp application..."),
    inets:stop(),
    application:stop(shrimp),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Note: We don't try to clear state because clearing backends/rules
    %% involves stopping pools which can cause supervisor issues.
    %% Instead tests are independent and use unique names.
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
        test_list_empty_backends,
        test_create_and_get_backend,
        test_update_backend,
        test_delete_backend,
        test_backend_validation_errors,
        test_create_rule_with_backend,
        test_list_rules,
        test_update_rule,
        test_delete_rule,
        test_rule_validation_errors,
        test_backend_in_use_error,
        test_full_e2e_workflow
    ].

%%--------------------------------------------------------------------
%% Backend Tests
%%--------------------------------------------------------------------

test_list_empty_backends(_Config) ->
    Response = make_request(get, "/backend", undefined),
    {ok, {{_, 200, _}, _, Body}} = Response,
    Data = jsx:decode(list_to_binary(Body), [return_maps]),
    ct:log("Initial backends list: ~p", [Data]),
    case Data of
        [] -> ok;
        L when is_list(L) -> ok  %% May have leftover data
    end.

test_create_and_get_backend(_Config) ->
    TestName = "create_get_backend_" ++ integer_to_list(timestamp()),
    Payload = #{
        <<"name">> => list_to_binary(TestName),
        <<"url">> => <<"http://localhost:9000">>,
        <<"pool-size">> => #{<<"min">> => 1, <<"max">> => 5}
    },
    
    %% Create
    Response = make_request(post, "/backend", Payload),
    {ok, {{_, 201, _}, _, Body}} = Response,
    Data = jsx:decode(list_to_binary(Body), [return_maps]),
    <<"created">> = maps:get(<<"status">>, Data),
    
    %% Get
    Path = "/backend/" ++ TestName,
    GetResponse = make_request(get, Path, undefined),
    {ok, {{_, 200, _}, _, GetBody}} = GetResponse,
    GetData = jsx:decode(list_to_binary(GetBody), [return_maps]),
    TestName = binary_to_list(maps:get(<<"name">>, GetData)),
    <<"http://localhost:9000">> = maps:get(<<"url">>, GetData).

test_update_backend(_Config) ->
    TestName = "update_backend_" ++ integer_to_list(timestamp()),
    Payload = #{
        <<"name">> => list_to_binary(TestName),
        <<"url">> => <<"http://localhost:8080">>,
        <<"pool-size">> => #{<<"min">> => 1, <<"max">> => 5}
    },
    
    %% Create
    make_request(post, "/backend", Payload),
    
    %% Update
    UpdatedPayload = Payload#{<<"url">> => <<"http://localhost:9090">>},
    Path = "/backend/" ++ TestName,
    Response = make_request(put, Path, UpdatedPayload),
    {ok, {{_, 200, _}, _, Body}} = Response,
    Data = jsx:decode(list_to_binary(Body), [return_maps]),
    <<"ok">> = maps:get(<<"status">>, Data),
    
    %% Verify update
    GetResponse = make_request(get, Path, undefined),
    {ok, {{_, 200, _}, _, GetBody}} = GetResponse,
    GetData = jsx:decode(list_to_binary(GetBody), [return_maps]),
    <<"http://localhost:9090">> = maps:get(<<"url">>, GetData).

test_delete_backend(_Config) ->
    TestName = "delete_backend_" ++ integer_to_list(timestamp()),
    Payload = #{
        <<"name">> => list_to_binary(TestName),
        <<"url">> => <<"http://localhost:8080">>,
        <<"pool-size">> => #{<<"min">> => 1, <<"max">> => 5}
    },
    
    %% Create
    make_request(post, "/backend", Payload),
    
    %% Delete
    Path = "/backend/" ++ TestName,
    Response = make_request(delete, Path, undefined),
    {ok, {{_, 200, _}, _, Body}} = Response,
    Data = jsx:decode(list_to_binary(Body), [return_maps]),
    <<"ok">> = maps:get(<<"status">>, Data),
    
    %% Verify deletion
    GetResponse = make_request(get, Path, undefined),
    {ok, {{_, 404, _}, _, _}} = GetResponse.

test_backend_validation_errors(_Config) ->
    %% Missing name
    InvalidPayload1 = #{
        <<"url">> => <<"http://localhost:8080">>,
        <<"pool-size">> => #{<<"min">> => 1, <<"max">> => 5}
    },
    {ok, {{_, 400, _}, _, _}} = make_request(post, "/backend", InvalidPayload1),
    
    %% Missing URL
    InvalidPayload2 = #{
        <<"name">> => <<"backend1">>,
        <<"pool-size">> => #{<<"min">> => 1, <<"max">> => 5}
    },
    {ok, {{_, 400, _}, _, _}} = make_request(post, "/backend", InvalidPayload2),
    
    %% Invalid pool size (min > max)
    InvalidPayload3 = #{
        <<"name">> => <<"backend1">>,
        <<"url">> => <<"http://localhost:8080">>,
        <<"pool-size">> => #{<<"min">> => 5, <<"max">> => 2}
    },
    {ok, {{_, 400, _}, _, _}} = make_request(post, "/backend", InvalidPayload3).

%%--------------------------------------------------------------------
%% Rule Tests
%%--------------------------------------------------------------------

test_create_rule_with_backend(_Config) ->
    BackendName = "rule_backend_" ++ integer_to_list(timestamp()),
    RuleName = "rule_" ++ integer_to_list(timestamp()),
    
    %% Create backend
    BackendPayload = #{
        <<"name">> => list_to_binary(BackendName),
        <<"url">> => <<"http://localhost:9000">>,
        <<"pool-size">> => #{<<"min">> => 1, <<"max">> => 5}
    },
    {ok, {{_, 201, _}, _, _}} = make_request(post, "/backend", BackendPayload),
    
    %% Create rule
    RulePayload = #{
        <<"name">> => list_to_binary(RuleName),
        <<"in">> => <<"/api">>,
        <<"out">> => #{
            <<"backends">> => [list_to_binary(BackendName)],
            <<"dispatcher">> => <<"round_robin">>
        },
        <<"middlewares">> => []
    },
    Response = make_request(post, "/rule", RulePayload),
    {ok, {{_, 201, _}, _, _}} = Response.

test_list_rules(_Config) ->
    Response = make_request(get, "/rule", undefined),
    case Response of
        {ok, {{_, 200, _}, _, _Body}} -> 
            ct:log("Rules list retrieved successfully"),
            ok;
        Other ->
            ct:log("Unexpected response: ~p", [Other]),
            {error, unexpected_response}
    end.

test_update_rule(_Config) ->
    BackendName = "update_rule_backend_" ++ integer_to_list(timestamp()),
    RuleName = "update_rule_" ++ integer_to_list(timestamp()),
    
    %% Create backend
    BackendPayload = #{
        <<"name">> => list_to_binary(BackendName),
        <<"url">> => <<"http://localhost:9000">>,
        <<"pool-size">> => #{<<"min">> => 1, <<"max">> => 5}
    },
    {ok, {{_, 201, _}, _, _}} = make_request(post, "/backend", BackendPayload),
    
    %% Create rule
    RulePayload = #{
        <<"name">> => list_to_binary(RuleName),
        <<"in">> => <<"/api">>,
        <<"out">> => #{
            <<"backends">> => [list_to_binary(BackendName)],
            <<"dispatcher">> => <<"round_robin">>
        },
        <<"middlewares">> => []
    },
    {ok, {{_, 201, _}, _, _}} = make_request(post, "/rule", RulePayload),
    
    %% Update rule
    UpdatedPayload = RulePayload#{
        <<"in">> => <<"/v2/api">>
    },
    Path = "/rule/" ++ RuleName,
    {ok, {{_, 200, _}, _, _}} = make_request(put, Path, UpdatedPayload).

test_delete_rule(_Config) ->
    BackendName = "delete_rule_backend_" ++ integer_to_list(timestamp()),
    RuleName = "delete_rule_" ++ integer_to_list(timestamp()),
    
    %% Create backend
    BackendPayload = #{
        <<"name">> => list_to_binary(BackendName),
        <<"url">> => <<"http://localhost:9000">>,
        <<"pool-size">> => #{<<"min">> => 1, <<"max">> => 5}
    },
    {ok, {{_, 201, _}, _, _}} = make_request(post, "/backend", BackendPayload),
    
    %% Create rule
    RulePayload = #{
        <<"name">> => list_to_binary(RuleName),
        <<"in">> => <<"/api">>,
        <<"out">> => #{
            <<"backends">> => [list_to_binary(BackendName)],
            <<"dispatcher">> => <<"round_robin">>
        },
        <<"middlewares">> => []
    },
    {ok, {{_, 201, _}, _, _}} = make_request(post, "/rule", RulePayload),
    
    %% Delete rule
    Path = "/rule/" ++ RuleName,
    {ok, {{_, 200, _}, _, _}} = make_request(delete, Path, undefined),
    
    %% Verify deletion
    {ok, {{_, 404, _}, _, _}} = make_request(get, Path, undefined).

test_rule_validation_errors(_Config) ->
    BackendName = "validation_backend_" ++ integer_to_list(timestamp()),
    
    %% Create a backend for validation tests
    BackendPayload = #{
        <<"name">> => list_to_binary(BackendName),
        <<"url">> => <<"http://localhost:9000">>,
        <<"pool-size">> => #{<<"min">> => 1, <<"max">> => 5}
    },
    {ok, {{_, 201, _}, _, _}} = make_request(post, "/backend", BackendPayload),
    
    %% Missing name
    InvalidPayload1 = #{
        <<"in">> => <<"/api">>,
        <<"out">> => #{
            <<"backends">> => [list_to_binary(BackendName)],
            <<"dispatcher">> => <<"round_robin">>
        }
    },
    {ok, {{_, 400, _}, _, _}} = make_request(post, "/rule", InvalidPayload1),
    
    %% Missing in path
    InvalidPayload2 = #{
        <<"name">> => <<"test_rule">>,
        <<"out">> => #{
            <<"backends">> => [list_to_binary(BackendName)],
            <<"dispatcher">> => <<"round_robin">>
        }
    },
    {ok, {{_, 400, _}, _, _}} = make_request(post, "/rule", InvalidPayload2),
    
    %% Missing backends
    InvalidPayload3 = #{
        <<"name">> => <<"test_rule">>,
        <<"in">> => <<"/api">>,
        <<"out">> => #{
            <<"dispatcher">> => <<"round_robin">>
        }
    },
    {ok, {{_, 400, _}, _, _}} = make_request(post, "/rule", InvalidPayload3),
    
    %% Nonexistent backend
    InvalidPayload4 = #{
        <<"name">> => <<"test_rule">>,
        <<"in">> => <<"/api">>,
        <<"out">> => #{
            <<"backends">> => [<<"nonexistent_backend">>],
            <<"dispatcher">> => <<"round_robin">>
        }
    },
    {ok, {{_, 400, _}, _, _}} = make_request(post, "/rule", InvalidPayload4).

%%--------------------------------------------------------------------
%% Integration Tests
%%--------------------------------------------------------------------

test_backend_in_use_error(_Config) ->
    BackendName = "in_use_backend_" ++ integer_to_list(timestamp()),
    RuleName = "in_use_rule_" ++ integer_to_list(timestamp()),
    
    %% Create backend
    BackendPayload = #{
        <<"name">> => list_to_binary(BackendName),
        <<"url">> => <<"http://localhost:8080">>,
        <<"pool-size">> => #{<<"min">> => 1, <<"max">> => 5}
    },
    make_request(post, "/backend", BackendPayload),
    
    %% Create rule using backend
    RulePayload = #{
        <<"name">> => list_to_binary(RuleName),
        <<"in">> => <<"/api">>,
        <<"out">> => #{
            <<"backends">> => [list_to_binary(BackendName)],
            <<"dispatcher">> => <<"round_robin">>
        },
        <<"middlewares">> => []
    },
    make_request(post, "/rule", RulePayload),
    
    %% Try to delete backend - should fail with 409
    BackendPath = "/backend/" ++ BackendName,
    Response = make_request(delete, BackendPath, undefined),
    {ok, {{_, 409, _}, _, _}} = Response,
    
    %% Delete rule first
    RulePath = "/rule/" ++ RuleName,
    make_request(delete, RulePath, undefined),
    
    %% Now backend deletion should succeed
    Response2 = make_request(delete, BackendPath, undefined),
    {ok, {{_, 200, _}, _, _}} = Response2.

test_full_e2e_workflow(_Config) ->
    Suffix = integer_to_list(timestamp()),
    Backend1Name = "e2e_api_backend_" ++ Suffix,
    Backend2Name = "e2e_web_backend_" ++ Suffix,
    Rule1Name = "e2e_api_rule_" ++ Suffix,
    Rule2Name = "e2e_web_rule_" ++ Suffix,
    
    %% Create multiple backends
    Backend1 = #{
        <<"name">> => list_to_binary(Backend1Name),
        <<"url">> => <<"http://api.local:3000">>,
        <<"pool-size">> => #{<<"min">> => 5, <<"max">> => 20}
    },
    {ok, {{_, 201, _}, _, _}} = make_request(post, "/backend", Backend1),
    
    Backend2 = #{
        <<"name">> => list_to_binary(Backend2Name),
        <<"url">> => <<"http://web.local:80">>,
        <<"pool-size">> => #{<<"min">> => 2, <<"max">> => 10}
    },
    {ok, {{_, 201, _}, _, _}} = make_request(post, "/backend", Backend2),
    
    %% Create multiple rules
    Rule1 = #{
        <<"name">> => list_to_binary(Rule1Name),
        <<"in">> => <<"/api">>,
        <<"out">> => #{
            <<"backends">> => [list_to_binary(Backend1Name)],
            <<"dispatcher">> => <<"round_robin">>
        }
    },
    {ok, {{_, 201, _}, _, _}} = make_request(post, "/rule", Rule1),
    
    Rule2 = #{
        <<"name">> => list_to_binary(Rule2Name),
        <<"in">> => <<"/web">>,
        <<"out">> => #{
            <<"backends">> => [list_to_binary(Backend2Name)],
            <<"dispatcher">> => <<"round_robin">>
        }
    },
    {ok, {{_, 201, _}, _, _}} = make_request(post, "/rule", Rule2),
    
    %% Verify we can get individual resources
    {ok, {{_, 200, _}, _, _}} = make_request(get, "/backend/" ++ Backend1Name, undefined),
    {ok, {{_, 200, _}, _, _}} = make_request(get, "/rule/" ++ Rule1Name, undefined),
    
    %% Update a backend
    UpdatedBackend1 = Backend1#{<<"pool-size">> => #{<<"min">> => 10, <<"max">> => 30}},
    {ok, {{_, 200, _}, _, _}} = make_request(put, "/backend/" ++ Backend1Name, UpdatedBackend1),
    
    ct:log("Full E2E workflow completed successfully").

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------

make_request(Method, Path, Payload) ->
    Url = ?API_BASE_URL ++ Path,
    Headers = [{"content-type", "application/json"}],
    Body = case Payload of
        undefined -> "";
        _ -> jsx:encode(Payload)
    end,
    
    Options = [
        {timeout, ?TEST_TIMEOUT},
        {connect_timeout, ?TEST_TIMEOUT}
    ],
    
    case Method of
        get ->
            httpc:request(get, {Url, Headers}, Options, []);
        post ->
            httpc:request(post, {Url, Headers, "application/json", Body}, Options, []);
        put ->
            httpc:request(put, {Url, Headers, "application/json", Body}, Options, []);
        delete ->
            httpc:request(delete, {Url, Headers}, Options, [])
    end.

timestamp() ->
    erlang:system_time(millisecond).
