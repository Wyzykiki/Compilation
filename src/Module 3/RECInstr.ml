open RECExpr

type instruction =
  | Nop
  | Print of expression
  | Exit
  | Write of expression * expression
  | If    of expression * sequence * sequence
  | While of expression * sequence
  | Return of expression
  | MkArray of expression (* adresse *)
    * expression (* taille *) * expression (* élément de remplissage *)
  | NewRec of expression (* adresse *) * string (* nom *)
and sequence = instruction list
