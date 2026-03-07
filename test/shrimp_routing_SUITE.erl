-module(shrimp_routing_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-define(MECK_MODS, [
                    shrimp_model
                   ]).

suite() ->
  [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
  meck:new(?MECK_MODS, [passthrough]),
  Config.

end_per_suite(_Config) ->
  meck:unload(),
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
  meck:expect(shrimp_model, list_rules, fun() -> {ok, []} end),
  no_match = shrimp_router:pick_rule(#{path => <<"none">>}),
  ok.

test_routing_simple_match(_) ->
  Rule = #{name => <<"rule_1">>,
           'in' => <<"/api/kraken">>,
           out => #{backends => [<<"backend_1">>, <<"backend_2">>],
                    dispatcher => random},
           middlewares => []},

  meck:expect(shrimp_model, list_rules, fun() -> {ok, [Rule]} end),
 
  Rule = shrimp_router:pick_rule(#{path => <<"/api/kraken/borgen">>}),
  ok.

test_routing_no_match(_) -> 
  Rule = #{name => <<"rule_1">>,
           'in' => <<"/api/kraken">>,
           out => #{backends => [<<"backend_1">>, <<"backend_2">>],
                    dispatcher => random},
           middlewares => []},
  meck:expect(shrimp_model, list_rules, fun() -> {ok, [Rule]} end),
 
  no_match = shrimp_router:pick_rule(#{path => <<"/api/xkraken/borgen">>}),
  ok.

test_routing_match_first(_) -> 
  Rule = #{name => <<"rule_1">>,
           'in' => <<"/api/kraken">>,
           out => #{backends => [<<"backend_1">>, <<"backend_2">>],
                    dispatcher => random},
           middlewares => []},

  meck:expect(shrimp_model, list_rules, fun() -> 
                                            {ok, [Rule, Rule#{name => <<"rule_2">>}]} 
                                        end),
  Rule = shrimp_router:pick_rule(#{path => <<"/api/kraken/borgen">>}),
  ok.

test_routing_match_last(_) -> 
  Rule = #{name => <<"rule_1">>,
           'in' => <<"/api/kraken">>,
           out => #{backends => [<<"backend_1">>, <<"backend_2">>],
                    dispatcher => random},
           middlewares => []},

  meck:expect(shrimp_model, list_rules, fun() -> 
                                            {ok, [Rule#{name => <<"rule_2">>, 'in' => <<"/pepe/kraken">>}, Rule]} 
                                        end),
  Rule = shrimp_router:pick_rule(#{path => <<"/api/kraken/borgen">>}),
  ok.

test_routing_pick_backend(_) ->
  Rule = #{name => <<"rule_1">>,
           'in' => <<"/api/kraken">>,
           out => #{backends => [<<"backend_1">>, <<"backend_2">>],
                    dispatcher => random},
           middlewares => []},

  BackendBase = #{url => <<"http://backend/api/">>,
                  pool_size => #{min => 1, max =>2},
                  pid => pid_1},

  meck:expect(shrimp_model, get_backend, fun
                                           (<<"backend_1">> = Name) -> 
                                             {ok, BackendBase#{name => Name, 
                                                               pid => pid_1}};
                                           (<<"backend_2">> = Name) -> 
                                             {ok, BackendBase#{name => Name, 
                                                               pid => pid_2}}
                                         end),

  case shrimp_router:pick_backend(Rule) of
    {ok, pid_1}-> ok;
    {ok, pid_2}-> ok
  end,

  ok.

