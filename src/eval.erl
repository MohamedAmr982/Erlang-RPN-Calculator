-module(eval).
-include("common.hrl").

-export([eval/1]).

-doc """
input:
  Operand1
  Operand2
output:
  returns Operand1/Opernad2
""".
-spec eval_division(Operand1::float(), Operand2::float()) -> float().
eval_division(Operand1, Operand2) ->
  case Operand2 of
    +0.0 -> error(division_by_zero);
    -0.0 -> error(division_by_zero);
    _ -> Operand1 / Operand2
  end.


-doc """
input:
  Op: unary operator to be applied
  Stack: operands are pushed to/ popped from it
output:
  returns the updated stack after pushing the result
""".
-spec eval_unary_op(Op::common:op(), Stack::list()) -> list().
eval_unary_op(Op, [Operand | Stack_tail]) ->
  Result = case Op of
             ?SIGN_OP -> -1 * Operand;
             _ -> error(operator_not_unary)
           end,
  % pop the operand, push the result, then
  % return the updated stack
  [Result | Stack_tail].

-doc """
input:
input:
  Op: binary operator to be applied
  Stack: operands are pushed to/ popped from it
output:
  returns the updated stack after pushing the result
""".
-spec eval_binary_op(Op::common:op(), Stack::list()) -> list().
eval_binary_op(Op, [Operand2, Operand1 | Stack_tail]) ->
  Result = case Op of
             ?ADD_OP -> Operand1 + Operand2;
             ?SUB_OP -> Operand1 - Operand2;
             ?MUL_OP -> Operand1 * Operand2;
             ?DIV_OP -> eval_division(Operand1, Operand2);
             ?POW_OP -> math:pow(Operand1, Operand2);
             _ -> error(operator_not_binary)
           end,
  % pop the two operands, push the result, then
  % return the updated stack
  [Result | Stack_tail].


-doc """
input:
  Op: operator to be applied
  Stack: operands are pushed to/ popped from it
output:
  returns the updated stack after pushing the result
""".
-spec eval_op(Op::common:op(), Stack::list()) -> list().
eval_op(Op, Stack = [X, Y | _]) when is_number(X), is_number(Y)->
  case Op of
    ?SIGN_OP -> eval_unary_op(Op, Stack);
    _ -> eval_binary_op(Op, Stack)
  end;
eval_op(Op, Stack = [X | _]) when is_number(X) -> eval_unary_op(Op, Stack);
eval_op(_, _) -> error(invalid_operands).


-doc """
input:
  Rpe_list: RPN expression to be evaluated
  Stack: operands are pushed to/ popped from it
output:
  returns the value of the expression
Tail recursive implementation of eval/1
""".
-spec eval(Rpe_list::list(), Stack::list()) -> float().
eval([], [Result]) -> Result;

% the stack should contain a single element (the result)
% after the input had been consumed
eval([], _) -> error(invalid_expression);

eval([X | Y], Stack) ->
  case X of
    % push number to stack and continue
    _ when is_number(X) -> eval(Y, [X | Stack]);
    _ ->
      % evaluate operator and continue
      Updated_stack = eval_op(X, Stack),
      eval(Y, Updated_stack)
  end.


-doc """
input:
  Rpe_list: RPN expression to be evaluated
output:
  returns the value of the expression
Calls eval/2
""".
-spec eval(Rpe_list::list()) -> float().
eval(Rpe_list) -> eval(Rpe_list, []).
