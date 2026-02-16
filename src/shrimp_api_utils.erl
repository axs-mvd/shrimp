-module(shrimp_api_utils).

-export([reply_json/3, reply_error/3, error_to_status/1]).

reply_json(Req, StatusCode, Body) ->
  JsonBody = jsx:encode(Body),
  cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json">>}, JsonBody, Req).

reply_error(Req, StatusCode, Error) ->
  ErrorBody = #{<<"error">> => atom_to_binary(Error, utf8)},
  reply_json(Req, StatusCode, ErrorBody).

error_to_status(backend_already_exists) -> 200;
error_to_status(backend_not_found) -> 404;
error_to_status(backend_in_use) -> 409;
error_to_status(invalid_name) -> 400;
error_to_status(missing_url) -> 400;
error_to_status(missing_name) -> 400;
error_to_status(invalid_pool_size) -> 400;
error_to_status(rule_already_exists) -> 200;
error_to_status(rule_not_found) -> 404;
error_to_status(invalid_rule_name) -> 400;
error_to_status(missing_out) -> 400;
error_to_status(missing_in_path) -> 400;
error_to_status(missing_dispatcher) -> 400;
error_to_status(missing_backends_in_out) -> 400;
error_to_status(rule_name_mismatch) -> 400;
error_to_status(backend_name_mismatch) -> 400;
error_to_status(_) -> 500.
