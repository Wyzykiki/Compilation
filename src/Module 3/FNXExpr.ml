type expression =
  | Immediate of int
  | Name  of string
  | Unop  of Op.unop * expression
  | Binop of Op.binop * expression * expression
  | Deref of expression (* adresse *)
  | Call  of expression (* fonction *) * expression list (* paramètres *)
