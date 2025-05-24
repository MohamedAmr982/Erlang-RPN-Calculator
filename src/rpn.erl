-module(rpn).
-include("common.hrl").

-export([to_rpn/1]).


-doc """
input:
  Op: operator token
  Rpe_list: RPN list being constructed
  Stack: operators, according to their precedence are pushed to/ popped from
    the stack while gerenating the RPN expression
output:
  returns a tuple, the updated Rpn_list and Stack after handling the operator.
""".
-spec handle_op(
    Op::common:op(),
    Rpe_list::list(),
    Stack::list()
) -> {list(), list()}.
handle_op(Op, Rpe_list, []) -> {Rpe_list, [Op]};
% only the sign operator can be pushed on top of higher precedence operators.
% sign operators can be pushed on top of each other.
handle_op(Op = {sign, _}, Rpe_list, Stack) -> {Rpe_list, [Op | Stack]};
handle_op(Op = {_, Precedence}, Rpe_list, Stack = [Top | Stack_tail]) ->
  case Top of
    % pop all higher/equal precedence operators, push them
    % to Rpe_list until Stack is empty, (, or a less
    % precedence operation is encountered
    {_, Other_prec} when Precedence >= Other_prec ->
      handle_op(Op, [Top | Rpe_list], Stack_tail);
    % push the operand to the stack
    {_, _} -> {Rpe_list, [Op | Stack]};
    ?OPENING_PAREN -> {Rpe_list, [Op | Stack]}
  end.


-doc """
input:
  Rpe_list: RPN list being constructed,
  Stack: operators, according to their precedence are pushed to/ popped from
    the stack while gerenating the RPN expression
output:
  returns a tuple, the updated Rpn_list and Stack after handling the closing
  parenthesis.
""".
-spec handle_closing_paren(Rpe_list::list(), Stack::list()) -> {list(), list()}.
handle_closing_paren(_, []) -> error(?UNBALANCED_PAREN);
handle_closing_paren(Rpe_list, [Top | Stack_tail]) ->
  case Top of
    ?OPENING_PAREN -> {Rpe_list, Stack_tail};
    _ -> handle_closing_paren([Top | Rpe_list], Stack_tail)
  end.


-doc """
input:
  X: number token
  Rpe_list: RPN list being constructed,
  Stack: operators, according to their precedence are pushed to/ popped from
    the stack while gerenating the RPN expression
output:
  returns a tuple, the updated Rpn_list and Stack after handling the closing
  parenthesis.
""".
-spec handle_number(
    X::number(),
    Rpe_list::list(),
    Stack::list()
) -> {list(), list()}.
handle_number(X, Rpe_list, Stack) -> {[X | Rpe_list], Stack}.



-doc """
input:
  Rpe_list: Reverse polish notation of the input tokens,
  Stack: operators, according to their precedence are pushed to/ popped from
    the stack while gerenating the RPN expression
output:
  returns a tuple, the updated Rpn_list and Stack after handling the opening
  parenthesis.
""".
-spec handle_opening_paren(Rpn_list::list(), Stack::list()) -> {list(), list()}.
handle_opening_paren(Rpe_list, Stack) -> {Rpe_list, [?OPENING_PAREN | Stack]}.

-doc """
input:
  Exp_tokens: input tokens
  Rpe_list: RPN list being constructed
  Stack: operators, according to their precedence are pushed to/ popped from
    the stack while gerenating the RPN expression
Tail recursive implementation of to_rpn/1
""".
-spec to_rpn(Exp_tokens::list(), Rpe_list::list(), Stack::list()) -> list().
to_rpn([], Rpe_list, []) -> Rpe_list;

% no more input, pop all items from the stack
% except if the stack contains opening parens
% then parens are unbalanced
to_rpn([], Rpe_list, [Top | Stack_tail]) ->
  case Top of
    ?OPENING_PAREN -> error(?UNBALANCED_PAREN);
    _ -> to_rpn([], [Top | Rpe_list], Stack_tail)
  end;

to_rpn([X | Y], Rpe_list, Stack) ->
  {Updated_rpe_list, Updated_stack} = case X of
                                        ?OPENING_PAREN -> handle_opening_paren(Rpe_list, Stack);
                                        ?CLOSING_PAREN -> handle_closing_paren(Rpe_list, Stack);
                                        {_, _} -> handle_op(X, Rpe_list, Stack);
                                        _ -> handle_number(X, Rpe_list, Stack)
                                      end,
  to_rpn(Y, Updated_rpe_list, Updated_stack).

-doc """
input:
  Exp_tokens: input tokens
output:
  list of tokens in RPN
""".
-spec to_rpn(Exp_tokens::list()) -> list().
to_rpn(Exp_tokens) -> lists:reverse(to_rpn(Exp_tokens, [], [])).
