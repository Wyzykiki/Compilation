open IMPExpr

type instruction =
  | Nop
  | Print of expression
  | Exit
  | Write of expression * expression
  | If    of expression * sequence * sequence
  | While of expression * sequence
  | Return of expression
  | Call of expression * expression * expression list
  (* Déclaration et initialisation d'une variable locale *)
  | NewVar of string * expression
and sequence = instruction list
