type expression =
  | Immediate of int
  | Name  of string
  | Unop  of Op.unop * expression
  | Binop of Op.binop * expression * expression
  | Deref of expression
  | Call  of expression * expression list
  | TupleAccess of expression (* base *) * expression (* décalage *)
