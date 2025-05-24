-module(token_tests).

-include("../src/common.hrl").
-include_lib("eunit/include/eunit.hrl").

% number tests

single_digit_number_test() ->
  ?assertEqual([9.0], token:tokenize("9")).

multi_digit_number_test() ->
  ?assertEqual([12.0], token:tokenize("12")).

decimal_number_test() ->
  ?assertEqual([2.5], token:tokenize("2.5")).

digit_ending_with_decimal_point_test() ->
  ?assertEqual([2.0], token:tokenize("2.")).

number_ending_with_decimal_point_test() ->
  ?assertEqual([84.0], token:tokenize("84.")).

decimal_point_then_digit_test() ->
  ?assertEqual([0.5], token:tokenize(".5")).

number_starting_with_decimal_pt_test() ->
  ?assertEqual([0.25], token:tokenize(".25")).

dot_test() -> ?assertEqual([0.0], token:tokenize(".")).

multiple_decimal_pt_test() ->
  ?assertError(?INVALID_NUMBER, token:tokenize("2.5.4")).

two_digits_and_space_test() ->
  ?assertEqual([2.0, 3.0], token:tokenize("2 3")).

two_numbers_and_space_test() ->
  ?assertEqual([2.35, 3.1415], token:tokenize("2.35    3.1415")).

% symbols tests

opening_bracket_test() ->
  ?assertEqual([$(], token:tokenize("(")).

closing_bracket_test() ->
  ?assertEqual([$)], token:tokenize(")")).

power_test() ->
  ?assertEqual([?POW_OP], token:tokenize("^")).

mul_test() ->
  ?assertEqual([?MUL_OP], token:tokenize("*")).

div_test() ->
  ?assertEqual([?DIV_OP], token:tokenize("/")).

add_test() ->
  ?assertEqual([?ADD_OP], token:tokenize("+")).

undefined_symbols_are_invalid_test() ->
  ?assertError(?UNKNOWN_SYMBOL, token:tokenize("a")).

% minus symbol tests

minus_at_exp_start_is_sign_test() ->
  ?assertEqual([?SIGN_OP], token:tokenize("-")).

minus_after_opening_paren_is_sign_test() ->
  ?assertEqual([?OPENING_PAREN, ?SIGN_OP], token:tokenize("(-")).

minus_after_closing_paren_is_sub_test() ->
  ?assertEqual([?CLOSING_PAREN, ?SUB_OP], token:tokenize(")-")).

minus_after_op_is_sign_test() ->
  ?assertEqual([?MUL_OP, ?SIGN_OP], token:tokenize("*-")).

minus_after_number_is_sub_test() ->
  ?assertEqual([12.0, ?SUB_OP], token:tokenize("12.0-")).

minus_resolution_does_not_depend_on_whitespace_test() ->
  ?assertEqual([12.0, ?SUB_OP], token:tokenize("12.0 -")),
  ?assertEqual([?OPENING_PAREN, ?SIGN_OP], token:tokenize("(  -")).

% whole expression tests

simple_expression_test() ->
  ?assertEqual(
    [3.14, ?ADD_OP, 45.0, ?MUL_OP, 0.5],
    token:tokenize("3.14  + 45.0* .5")
  ).

simple_expression_with_undefined_symbol_test() ->
  ?assertError(
    ?UNKNOWN_SYMBOL,
    token:tokenize(token:tokenize("3.14a  + 45.0* .5"))
  ).

compound_expression_test() ->
  ?assertEqual(
    [7.0, ?DIV_OP, ?OPENING_PAREN, 2.5, ?MUL_OP,
      ?OPENING_PAREN, 2.0, ?POW_OP, ?OPENING_PAREN, ?SIGN_OP, 3.0,
      ?ADD_OP, 9.81, ?SUB_OP, 1.0,
      ?CLOSING_PAREN, ?CLOSING_PAREN, ?CLOSING_PAREN],
    token:tokenize("7 / (2.5 * (2^(-3+9.81 - 1)))")
  ).


compound_expression_with_unknown_symbol_test() ->
  ?assertError(
    ?UNKNOWN_SYMBOL,
    token:tokenize("7 / (2.5 x (2^(3+9.81)))")
  ).
