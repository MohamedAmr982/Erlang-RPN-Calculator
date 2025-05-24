-module(token).
-include("common.hrl").
-export([tokenize/1]).


-doc """
input:
  Token_lst: generated tokens
output:
  generates either a negative sign or a subtraction operator
  according to the last generated token in Token_lst
""".
-spec handle_minus_token(Token_lst::list()) -> common:op().
handle_minus_token([]) -> ?SIGN_OP;
handle_minus_token([Prev_token | _]) ->
  case Prev_token of
    ?OPENING_PAREN -> ?SIGN_OP;
    ?CLOSING_PAREN -> ?SUB_OP;
    % another operator
    {_, _} -> ?SIGN_OP;
    % number
    _  -> ?SUB_OP
  end.

-doc """
input:
  Op_char: operator to be tokenized
  Token_lst: list of generated tokens
output:
  Returns an operator token given the integer representation of it.
  The list of all generated tokens is also required in order to
  differentiate between subtraction and the negative sign
""".
-spec tokenize_op(Op_char::char(), Token_lst::list()) -> common:op().
tokenize_op(Op_char, Token_lst) ->
  case Op_char of
    $^ -> ?POW_OP;
    $* -> ?MUL_OP;
    $/ -> ?DIV_OP;
    $+ -> ?ADD_OP;
    $- -> handle_minus_token(Token_lst);
    _ -> error(?UNKNOWN_SYMBOL)
  end.

-doc """
input:
  Digit_lst: list of characters of the number being processed
output:
  converts Digit_lst to a number
""".
-spec finalize_number(Digit_lst::list()) -> float().
finalize_number(Digit_lst = [X | _]) ->
  case X of
    % adds an implicit 0 for input numbers ending with a decimal point like '31.'
    $. -> list_to_float(lists:reverse([$0 | Digit_lst]));
    _ -> list_to_float(lists:reverse(Digit_lst))
  end.


-doc """
input:
  Exp_str: next characters in the input expression to be tokenized
  Digit_lst: list of characters of the number being processed
  Is_decimal: indicates whether the number being processed already contains
    a decimal point or not
output:
  returns a tuple: the numerical value of the number after reading it and a list
  of the remaining input expression
Tail recursive implementation of tokenize_number/1
""".
-spec tokenize_number(
    Exp_str::list(),
    Digit_lst::list(),
    Is_decimal::boolean()
) -> {float(), list()}.
tokenize_number([], Digit_lst, Is_decimal) ->
  % no more digits (end of input expression)
  case Is_decimal of
    true -> {finalize_number(Digit_lst), []};
    % adds ".0" to be able to convert the list to a float
    false -> {finalize_number([$0, $. | Digit_lst]), []}
  end;
tokenize_number(Exp_str = [X | Y], Digit_lst, Is_decimal) ->
  case X of
    _ when X >= $0, X =< $9 -> tokenize_number(Y, [X | Digit_lst], Is_decimal);
    $. when not Is_decimal -> tokenize_number(Y, [X | Digit_lst], true);
    % number must have at most one decimal point
    $. when Is_decimal -> error(?INVALID_NUMBER);
    % no more digits (whitespaces, operators, etc are next)
    _ when not Is_decimal->
      % adds ".0" to be able to convert the list to a float
      Number = finalize_number([$0, $. | Digit_lst]),
      {Number, Exp_str};
    _ ->
      Number = finalize_number(Digit_lst),
      {Number, Exp_str}
  end.

-doc """
input:
  Exp: input expression
output:
  returns a tuple, the numerical value of the number whose first character
  is the head of Exp, and the rest of the expression after parsing the number
Calls tokenize_number/3
""".
-spec tokenize_number(Exp::list()) -> {float(), list()}.
tokenize_number(Exp = [X | _]) ->
  case X of
    $. -> tokenize_number(Exp, [$0], false);
    _ -> tokenize_number(Exp, [], false)
  end.


-doc """
input:
  Exp: input expression
  Token_lst: list of tokens to be generated
output:
  list of generated tokens after processing Exp
Tail recursive implementation of tokenize/1
""".
-spec tokenize(Exp::list(), Token_lst::list()) -> list().
tokenize([], Token_lst) -> Token_lst;
tokenize(Exp = [X | Y], Token_lst) ->
  case X of
    _ when X >= $0, X =< $9; X =:= $. ->
      {Number, Rest_of_Exp} = tokenize_number(Exp),
      tokenize(Rest_of_Exp, [Number | Token_lst]);
    ?OPENING_PAREN -> tokenize(Y, [X | Token_lst]);
    ?CLOSING_PAREN -> tokenize(Y, [X | Token_lst]);
    $\s -> tokenize(Y, Token_lst);
    _ ->
      Op_token = tokenize_op(X, Token_lst),
      tokenize(Y, [Op_token | Token_lst])
  end.


-doc """
input:
  Exp: input expression
output:
  list of generated tokens after processing Exp
Calls tokenize/2
""".
-spec tokenize(Exp_str::list()) -> list().
tokenize(Exp_str) -> lists:reverse(tokenize(Exp_str, [])).

