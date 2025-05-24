-module(eval_tests).

-include("../src/common.hrl").
-include_lib("eunit/include/eunit.hrl").

% unary op tests

sign_op_test() ->
  ?assertEqual(-2.0, eval:eval([2.0, ?SIGN_OP])).

multiple_sign_op_test() ->
  ?assertEqual(2.0, eval:eval([2.0, ?SIGN_OP, ?SIGN_OP])).

invalid_unary_op_test() ->
  ?assertError(?OPERATOR_NOT_UNARY, eval:eval([2.0, ?ADD_OP])).

% binary op tests

add_op_test() ->
  ?assertEqual(5.0, eval:eval([3.0, 2.0, ?ADD_OP])).

sub_op_test() ->
  ?assertEqual(2.0, eval:eval([4.0, 2.0, ?SUB_OP])).

mul_op_test() ->
  ?assertEqual(27.0, eval:eval([3.0, 9.0, ?MUL_OP])).

div_op_test() ->
  ?assertEqual(9.0, eval:eval([27.0, 3.0, ?DIV_OP])).

division_by_zero_throws_error_test() ->
  ?assertError(?DIVISION_BY_ZERO, eval:eval([1.0, 0.0, ?DIV_OP])),
  ?assertError(?DIVISION_BY_ZERO, eval:eval([0.0, 0.0, ?DIV_OP])).

pow_op_test() ->
  ?assertEqual(9.0, eval:eval([3.0, 2.0, ?POW_OP])).

exp_errors_are_returned_test() ->
  ?assertError(?POWER_ERROR, eval:eval([-2.0, 0.5, ?POW_OP])).


% full expressions tests

valid_expression_returns_correct_result_test() ->
  ?assertEqual(
    -7.0,
    % -2 * 5 + 3
    eval:eval([2.0, ?SIGN_OP, 5.0, ?MUL_OP, 3.0, ?ADD_OP])).

missing_operands_expression_is_invalid_test() ->
  ?assertError(
    ?OPERATOR_NOT_UNARY,
    eval:eval([2.0, 3.0, ?POW_OP, 4.0, ?ADD_OP, ?SUB_OP])).

expression_did_not_yield_single_result_test() ->
  ?assertError(
    ?INVALID_EXPRESSION,
    eval:eval([2.0, 3.0, ?POW_OP, 4.0, ?SIGN_OP])).
