-module(shrimp_model_test).

-export([
    run_all_tests/0,
    test_backend_crud/0,
    test_rule_crud/0,
    test_referential_integrity/0,
    test_callbacks/0
]).

-define(TEST_TIMEOUT, 5000).

run_all_tests() ->
    io:format("~n=== Running Shrimp Model Tests ===~n~n", []),
    
    Tests = [
        {test_backend_crud, "Backend CRUD Operations"},
        {test_rule_crud, "Rule CRUD Operations"},
        {test_referential_integrity, "Referential Integrity"},
        {test_callbacks, "Callback Hooks"}
    ],
    
    Results = lists:map(fun({TestFun, TestName}) ->
        io:format("Running: ~s... ", [TestName]),
        try
            ?MODULE:TestFun(),
            io:format("✓ PASS~n", []),
            {TestFun, pass}
        catch
            Error:Reason ->
                io:format("✗ FAIL~n  Error: ~p~n", [{Error, Reason}]),
                {TestFun, fail}
        end
    end, Tests),
    
    Passed = length([ok || {_, pass} <- Results]),
    Total = length(Results),
    
    io:format("~n=== Test Summary ===~n", []),
    io:format("Passed: ~p/~p~n~n", [Passed, Total]),
    
    case Passed =:= Total of
        true -> io:format("All tests passed! ✓~n");
        false -> io:format("Some tests failed.~n")
    end.

test_backend_crud() ->
    setup_test(),
    
    %% Test add_backend
    Backend1 = #{
        name => "backend1",
        url => "http://localhost:8080",
        pool_size => #{min => 5, max => 20}
    },
    {ok, "backend1"} = shrimp_model:add_backend(Backend1),
    
    %% Test get_backend
    {ok, Retrieved} = shrimp_model:get_backend("backend1"),
    true = maps:get(name, Retrieved) =:= "backend1",
    
    %% Test modify_backend
    Backend1Modified = Backend1#{url => "http://localhost:9090"},
    ok = shrimp_model:modify_backend("backend1", Backend1Modified),
    {ok, Updated} = shrimp_model:get_backend("backend1"),
    "http://localhost:9090" = maps:get(url, Updated),
    
    %% Test list_backends
    Backend2 = #{
        name => "backend2",
        url => "http://localhost:8081",
        pool_size => #{min => 3, max => 10}
    },
    {ok, "backend2"} = shrimp_model:add_backend(Backend2),
    {ok, Backends} = shrimp_model:list_backends(),
    true = length(Backends) =:= 2,
    
    %% Test error conditions
    {error, backend_already_exists} = shrimp_model:add_backend(Backend1),
    {error, invalid_name} = shrimp_model:add_backend(Backend1#{name => 123}),
    BadBackend = maps:remove(url, Backend1),
    {error, missing_url} = shrimp_model:add_backend(BadBackend#{name => "test"}),
    NonexistentBackend = Backend1#{name => "nonexistent"},
    {error, backend_not_found} = shrimp_model:modify_backend("nonexistent", NonexistentBackend),
    
    cleanup_test().

test_rule_crud() ->
    setup_test(),
    
    %% Add backends first
    Backend1 = #{
        name => "api_backend",
        url => "http://api.local:3000",
        pool_size => #{min => 5, max => 20}
    },
    {ok, _} = shrimp_model:add_backend(Backend1),
    
    %% Test add_rule
    Rule1 = #{
        name => "rule_api",
        'in' => "/api/*",
        out => #{
            backends => ["api_backend"],
            dispatcher => round_robin
        },
        middlewares => []
    },
    {ok, "rule_api"} = shrimp_model:add_rule(Rule1),
    
    %% Test get_rule
    {ok, Retrieved} = shrimp_model:get_rule("rule_api"),
    "rule_api" = maps:get(name, Retrieved),
    
    %% Test modify_rule
    Rule1Modified = Rule1#{
        out => #{
            backends => ["api_backend"],
            dispatcher => least_connections
        }
    },
    ok = shrimp_model:modify_rule("rule_api", Rule1Modified),
    {ok, Updated} = shrimp_model:get_rule("rule_api"),
    least_connections = maps:get(dispatcher, maps:get(out, Updated)),
    
    %% Test list_rules
    Rule2 = #{
        name => "rule_static",
        'in' => "/static/*",
        out => #{
            backends => ["api_backend"],
            dispatcher => round_robin
        }
    },
    {ok, "rule_static"} = shrimp_model:add_rule(Rule2),
    {ok, Rules} = shrimp_model:list_rules(),
    true = length(Rules) =:= 2,
    
    %% Test error conditions
    {error, rule_already_exists} = shrimp_model:add_rule(Rule1),
    {error, invalid_rule_name} = shrimp_model:add_rule(Rule1#{name => 123}),
    BadRule = maps:remove('in', Rule1),
    {error, missing_in_path} = shrimp_model:add_rule(BadRule#{name => "new_rule"}),
    {error, {backend_not_found, "nonexistent"}} = shrimp_model:add_rule(#{
        name => "bad_rule",
        'in' => "/test/*",
        out => #{
            backends => ["nonexistent"],
            dispatcher => round_robin
        }
    }),
    
    cleanup_test().

test_referential_integrity() ->
    setup_test(),
    
    %% Add backend
    Backend = #{
        name => "backend1",
        url => "http://localhost:8080",
        pool_size => #{min => 5, max => 20}
    },
    {ok, _} = shrimp_model:add_backend(Backend),
    
    %% Add rule referencing the backend
    Rule = #{
        name => "rule1",
        'in' => "/api/*",
        out => #{
            backends => ["backend1"],
            dispatcher => round_robin
        }
    },
    {ok, _} = shrimp_model:add_rule(Rule),
    
    %% Try to remove backend in use - should fail
    {error, backend_in_use} = shrimp_model:remove_backend("backend1"),
    
    %% Remove the rule first
    ok = shrimp_model:remove_rule("rule1"),
    
    %% Now we can remove the backend
    ok = shrimp_model:remove_backend("backend1"),
    {error, not_found} = shrimp_model:get_backend("backend1"),
    
    cleanup_test().

test_callbacks() ->
    setup_test(),
    
    %% Register ourselves as callback listener
    CallbackPid = self(),
    ok = shrimp_model:register_callback(on_backend_added, CallbackPid),
    ok = shrimp_model:register_callback(on_rule_added, CallbackPid),
    
    %% Add a backend and check callback
    Backend = #{
        name => "test_backend",
        url => "http://localhost:8080",
        pool_size => #{min => 5, max => 20}
    },
    {ok, _} = shrimp_model:add_backend(Backend),
    receive
        {shrimp_model, on_backend_added, Data} ->
            {Name, _BackendData} = Data,
            "test_backend" = Name
    after ?TEST_TIMEOUT ->
        throw(callback_not_received)
    end,
    
    %% Add a rule and check callback
    Rule = #{
        name => "test_rule",
        'in' => "/test/*",
        out => #{
            backends => ["test_backend"],
            dispatcher => round_robin
        }
    },
    {ok, _} = shrimp_model:add_rule(Rule),
    receive
        {shrimp_model, on_rule_added, RuleData} ->
            {RuleName, _RuleDataMap} = RuleData,
            "test_rule" = RuleName
    after ?TEST_TIMEOUT ->
        throw(callback_not_received)
    end,
    
    %% Unregister callback
    ok = shrimp_model:unregister_callback(on_backend_added, CallbackPid),
    
    cleanup_test().

setup_test() ->
    case whereis(shrimp_model) of
        undefined ->
            {ok, _Pid} = shrimp_model:start_link();
        _Pid ->
            shrimp_model:stop(),
            timer:sleep(100),
            {ok, _PidNew} = shrimp_model:start_link()
    end.

cleanup_test() ->
    shrimp_model:stop(),
    timer:sleep(50).
