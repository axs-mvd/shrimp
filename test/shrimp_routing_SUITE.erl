-module(shrimp_routing_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

suite() ->
  [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

all() ->
  [
   test_routing_without_rules,
   test_routing_simple_match,
   test_routing_no_match,
   test_routing_match_first,
   test_routing_match_last,
   test_routing_pick_backend
  ].


test_routing_without_rules(_) ->
  no_match = shrimp_router:route(#{path => <<"none">>}, []),
  ok.

test_routing_simple_match(_) ->
  Rule = #{name => <<"rule_1">>,
           'in' => <<"/api/kraken">>,
           out => #{backends => [<<"backend_1">>, <<"backend_2">>],
                    dispatcher => random},
           middlewares => []},

  Rule = shrimp_router:route(#{path => <<"/api/kraken/borgen">>}, [Rule]),
  ok.

test_routing_no_match(_) -> 
  Rule = #{name => <<"rule_1">>,
           'in' => <<"/api/kraken">>,
           out => #{backends => [<<"backend_1">>, <<"backend_2">>],
                    dispatcher => random},
           middlewares => []},

  no_match = shrimp_router:route(#{path => <<"/api/xkraken/borgen">>}, [Rule]),
  ok.

test_routing_match_first(_) -> 
  Rule = #{name => <<"rule_1">>,
           'in' => <<"/api/kraken">>,
           out => #{backends => [<<"backend_1">>, <<"backend_2">>],
                    dispatcher => random},
           middlewares => []},

  Rule = shrimp_router:route(#{path => <<"/api/kraken/borgen">>}, [Rule, Rule#{name => <<"rule_2">>}]),
  ok.

test_routing_match_last(_) -> 
  Rule = #{name => <<"rule_1">>,
           'in' => <<"/api/kraken">>,
           out => #{backends => [<<"backend_1">>, <<"backend_2">>],
                    dispatcher => random},
           middlewares => []},

  Rule = shrimp_router:route(#{path => <<"/api/kraken/borgen">>}, [Rule#{name => <<"rule_2">>, 'in' => <<"/pepe/kraken">>}, Rule]),
  ok.

test_routing_pick_backend(_) ->
  Rule = #{name => <<"rule_1">>,
           'in' => <<"/api/kraken">>,
           out => #{backends => [<<"backend_1">>, <<"backend_2">>],
                    dispatcher => random},
           middlewares => []},

  Rule = shrimp_router:route(#{path => <<"/api/kraken/borgen">>}, [Rule#{name => <<"rule_2">>, 'in' => <<"/pepe/kraken">>}, Rule]),
  case shrimp_router:pick_backend(Rule) of
    <<"backend_1">> -> ok;
    <<"backend_2">> -> ok
  end,
  ok.

