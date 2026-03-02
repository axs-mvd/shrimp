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
        test_routing_pick_backend,
        test_routing_match_no_backend
    ].


test_routing_without_rules(_) ->
  no_match = shrimp_router:route(#{path => <<"none">>}, []),
  ok.

test_routing_simple_match(_) ->

%route(Req, [#{name := Name,
%              backends := _,
%              'in' := In} = Rule | Rules]) ->
 
  Rule = #{name => <<"rule_1">>,
           'in' => <<"/api/kraken">>,
           backends => [<<"backend_1">>, <<"backend_2">>],
           dispatcher => random,
           middlewares => []},

  {ok, Rule} = shrimp_router:route(#{path => <<"/api/kraken/borgen">>}, [Rule]),
  ok.

test_routing_no_match(_) -> ok.
test_routing_match_first(_) -> ok.
test_routing_match_last(_) -> ok.
test_routing_pick_backend(_) -> ok.
test_routing_match_no_backend(_) -> ok.


 
