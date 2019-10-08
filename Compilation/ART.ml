type instruction =
  | Nop
  | Print of IMPExpr.expression
  | Exit
  | Label    of string
  | Jump     of IMPExpr.expression (* adresse *)
  | JumpWhen of IMPExpr.expression (* adresse *)
              * IMPExpr.expression (* condition *)
  | Write of IMPExpr.expression (* adresse *)
           * IMPExpr.expression (* valeur *)
      
type program = { text: instruction list; data: (string * int) list }

let rec instr_to_string = function
  | Nop -> "nop;"
  | Print(e) -> "print(" ^ (IMPExpr.to_string e) ^ ");"
  | Exit -> "exit;"
  | Label(s) -> s ^ ":"
  | Jump(e) -> "jump " ^ (IMPExpr.to_string e) ^ ";"
  | JumpWhen(e, c) -> "jump " ^ (IMPExpr.to_string e) ^ " when " ^ (IMPExpr.to_string c) ^ ";"
  | Write(le, e) -> (IMPExpr.to_string le) ^ " := " ^ (IMPExpr.to_string e) ^ ";"
    
let rec seq_to_string = function
  | [] -> ""
  | i::s -> (instr_to_string i) ^ "\n" ^ (seq_to_string s)

let rec data_to_string = function
  | [] -> ""
  | (label, value)::s -> label ^ ": " ^ (string_of_int value) ^ "\n" ^ (data_to_string s)
    
let to_string p =
  ".text\n" ^ (seq_to_string p.text) ^ "\n.data\n" ^ (data_to_string p.data)
    
