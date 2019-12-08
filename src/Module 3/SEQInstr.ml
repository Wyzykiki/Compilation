open IMPExpr

type instruction =
  | Nop
  | Print of expression
  | Exit
  | Write of expression * expression
  | If    of expression * instruction * instruction
  | While of expression * instruction
  | Return of expression
  | Call of expression * expression * expression list
  | NewVar of string * expression
  (* Notion de séquence intégrée aux instructions *)
  | Seq of instruction * instruction

(* Raccourci syntaxique pour la concaténation d'instructions *)
let (@@) i1 i2 = Seq(i1, i2)
