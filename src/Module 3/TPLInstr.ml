open TPLExpr

type instruction =
  | Nop
  | Print of expression
  | Exit
  | Write of expression * expression
  | If    of expression * sequence * sequence
  | While of expression * sequence
  | Return of expression
  | MkTuple of expression (* adresse *) * expression (* taille *)
and sequence = instruction list
