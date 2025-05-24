-module(rpn_tests).

-include("../src/common.hrl").
-include_lib("eunit/include/eunit.hrl").

% precedence tests (no paren.)
power_has_highest_precedence_test() ->
  ?assertEqual([?POW_OP, ?MUL_OP], rpn:to_rpn([?MUL_OP, ?POW_OP])),
  ?assertEqual([?POW_OP, ?MUL_OP], rpn:to_rpn([?POW_OP, ?MUL_OP])).

mul_is_higher_than_add_test() ->
  ?assertEqual([?MUL_OP, ?ADD_OP], rpn:to_rpn([?ADD_OP, ?MUL_OP])),
  ?assertEqual([?MUL_OP, ?ADD_OP], rpn:to_rpn([?MUL_OP, ?ADD_OP])).

equal_precedence_operators_are_popped_in_order_test() ->
  ?assertEqual([?ADD_OP, ?SUB_OP], rpn:to_rpn([?ADD_OP, ?SUB_OP])),
  ?assertEqual([?SUB_OP, ?ADD_OP], rpn:to_rpn([?SUB_OP, ?ADD_OP])).

sign_op_is_pushed_on_top_of_any_op_test() ->
  ?assertEqual([?POW_OP, ?SIGN_OP], rpn:to_rpn([?SIGN_OP, ?POW_OP])).

% parenthesis tests

balanced_paren_are_allowed_test() ->
  ?assertEqual(
    [],
    % ()(()())
    rpn:to_rpn([?OPENING_PAREN, ?CLOSING_PAREN,
      ?OPENING_PAREN, ?OPENING_PAREN, ?CLOSING_PAREN, ?OPENING_PAREN,
      ?CLOSING_PAREN, ?CLOSING_PAREN])
  ).

unbalanced_paren_are_invalid_test() ->
  ?assertError(
    ?UNBALANCED_PAREN,
    % ()((())
    rpn:to_rpn([?OPENING_PAREN, ?CLOSING_PAREN,
      ?OPENING_PAREN, ?OPENING_PAREN, ?OPENING_PAREN,
      ?CLOSING_PAREN, ?CLOSING_PAREN])
  ).

exp_starting_with_closing_bracket_is_unbalanced_test() ->
  ?assertError(
    ?UNBALANCED_PAREN,
    % )(()())
    rpn:to_rpn([?CLOSING_PAREN,
      ?OPENING_PAREN, ?OPENING_PAREN, ?CLOSING_PAREN, ?OPENING_PAREN,
      ?CLOSING_PAREN, ?CLOSING_PAREN])
  ).

paren_have_highest_precedence_test() ->
  ?assertEqual(
    [?ADD_OP, ?POW_OP],
    rpn:to_rpn([?POW_OP, ?OPENING_PAREN, ?ADD_OP, ?CLOSING_PAREN])).

compound_paren_test() ->
  ?assertEqual(
    [?POW_OP, ?MUL_OP, ?ADD_OP, ?POW_OP],
    % ^ ( + ( * ^ ))
    rpn:to_rpn([?POW_OP, ?OPENING_PAREN, ?ADD_OP,
      ?OPENING_PAREN, ?MUL_OP, ?POW_OP, ?CLOSING_PAREN, ?CLOSING_PAREN])
  ).

unbalanced_expression_with_ops_test() ->
  ?assertError(
    ?UNBALANCED_PAREN,
    % ^ ( + ( * ^ )
    rpn:to_rpn([?POW_OP, ?OPENING_PAREN, ?ADD_OP,
      ?OPENING_PAREN, ?MUL_OP, ?POW_OP, ?CLOSING_PAREN])).

unbalanced_expression_with_ops_2_test() ->
  ?assertError(
    ?UNBALANCED_PAREN,
    % ^  + ( * ^ ))
    rpn:to_rpn([?POW_OP, ?ADD_OP,
      ?OPENING_PAREN, ?MUL_OP, ?POW_OP, ?CLOSING_PAREN, ?CLOSING_PAREN])).


% full expressions tests

simple_exp_test() ->
  ?assertEqual(
    [2.0, ?SIGN_OP, 5.0, ?MUL_OP, 3.0, ?ADD_OP],
    % -2.0 * 5 + 3
    rpn:to_rpn([?SIGN_OP, 2.0, ?MUL_OP, 5.0, ?ADD_OP, 3.0])
  ).

compound_exp_test() ->
  ?assertEqual(
    [7.0, 3.0, 4.0, ?SIGN_OP, 3.0, 2.0,
      ?DIV_OP, ?POW_OP, ?ADD_OP, ?DIV_OP],
    % 7 / (3 + (-4)^(3/2))
    rpn:to_rpn([7.0, ?DIV_OP, ?OPENING_PAREN, 3.0, ?ADD_OP,
      ?OPENING_PAREN, ?SIGN_OP, 4.0, ?CLOSING_PAREN,
      ?POW_OP, ?OPENING_PAREN, 3.0, ?DIV_OP, 2.0, ?CLOSING_PAREN, ?CLOSING_PAREN])).

extra_closing_paren_compound_exp_test() ->
  ?assertError(
    ?UNBALANCED_PAREN,
    rpn:to_rpn(% 7 / 3 + (-4)^(3/2))
      rpn:to_rpn([7.0, ?DIV_OP, 3.0, ?ADD_OP,
        ?OPENING_PAREN, ?SIGN_OP, 4.0, ?CLOSING_PAREN,
        ?POW_OP, ?OPENING_PAREN, 3.0, ?DIV_OP, 2.0, ?CLOSING_PAREN, ?CLOSING_PAREN]))
  ).

unbalanced_compound_exp_test() ->
  ?assertError(
    ?UNBALANCED_PAREN,
    % 7 / (3 + (-4)^(3/2)
    rpn:to_rpn([7.0, ?DIV_OP, ?OPENING_PAREN, 3.0, ?ADD_OP,
      ?OPENING_PAREN, ?SIGN_OP, 4.0, ?CLOSING_PAREN,
      ?POW_OP, ?OPENING_PAREN, 3.0, ?DIV_OP, 2.0, ?CLOSING_PAREN])).

