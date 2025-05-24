-ifndef(COMMON_HRL).
-define(COMMON_HRL, true).

-type op_type() :: add | sub | mul | divd | pow | sign.
-type op() :: {op_type(), number()}.

% operators
-define(POW_OP, {pow, 0}).
-define(SIGN_OP, {sign, 1}).
-define(MUL_OP, {mul, 2}).
-define(DIV_OP, {divd, 2}).
-define(ADD_OP, {add, 3}).
-define(SUB_OP, {sub, 3}).
-define(OPENING_PAREN, $().
-define(CLOSING_PAREN, $)).

% token errors
-define(UNKNOWN_SYMBOL, unknown_symbol).
-define(INVALID_NUMBER, invalid_number).




-endif.
