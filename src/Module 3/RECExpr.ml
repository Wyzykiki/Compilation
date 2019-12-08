type expression =
  | Immediate of int
  | Name  of string
  | Unop  of Op.unop * expression
  | Binop of Op.binop * expression * expression
  | Deref of expression
  | Call  of expression * expression list
  | ArrayAccess of expression (* base *) * expression (* indice *)
  | RecAccess of expression (* base *) * string (* champ *)
