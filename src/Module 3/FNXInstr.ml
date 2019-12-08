open FNXExpr

type instruction =
  | Nop
  | Print of expression
  | Exit
  | Write of expression * expression
  | If    of expression * sequence * sequence
  | While of expression * sequence
  | Return of expression
and sequence = instruction list
