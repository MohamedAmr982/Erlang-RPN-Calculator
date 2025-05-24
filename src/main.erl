-module(main).


-export([start/0]).

calc(Input_exp) ->
  try eval:eval(rpn:to_rpn(token:tokenize(Input_exp))) of
    Result -> {ok, Result}
  catch
    Error -> {error, Error}
  end.

start() ->
  {_, [Input_exp]} = io:fread(
    "Enter an expression, or type exit: ", "~s"),
  Result = case Input_exp of
    "exit" -> {ok, exit};
    _ -> calc(Input_exp)
  end,
  case Result of
    {_, exit} -> io:format("Bye~n");
    {ok, Number} when is_float(Number) ->
      io:format("~f~n", [Number]), start();
    _ -> Result, start()
  end.