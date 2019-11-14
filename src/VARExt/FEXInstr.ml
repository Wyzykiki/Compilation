type instruction =
  | Nop
  | Exit
  | Write of FEXExpr.expression * FEXExpr.expression
  | If    of FEXExpr.expression * sequence * sequence
  | While of FEXExpr.expression * sequence
  | Return of FEXExpr.expression
  | CreateVar of string * FEXExpr.expression
and sequence = instruction list

let rec sequence_to_string = function
  | [] -> ""
  | i::seq -> (instr_to_string i) ^ (sequence_to_string seq)

and instr_to_string = function
  | Nop -> ""
  | Exit -> "exit;\n"
  | Write(le, e) -> (FEXExpr.le_to_string le) ^ " := " ^ (FEXExpr.to_string e) ^ ";\n"
  | If(c, s1, s2) ->
    "if (" ^ (FEXExpr.to_string c) ^ ") {\n"
    ^ (sequence_to_string s1) ^ "} else {\n"
    ^ (sequence_to_string s2) ^ "}\n"
  | While(c, s) ->
    "while (" ^ (FEXExpr.to_string c) ^ ") {\n"
    ^ (sequence_to_string s) ^ "}\n"
  | Return(e) ->
    "return (" ^ (FEXExpr.to_string e) ^ ");\n"
  | CreateVar(lab, value) -> 
    lab ^ " := " ^ (FEXExpr.to_string value)

and args_to_string = function
  | [] -> ""
  | [a] -> FEXExpr.to_string a
  | a::args -> (FEXExpr.to_string a) ^ ", " ^ (args_to_string args)