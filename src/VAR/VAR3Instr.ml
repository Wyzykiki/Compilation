type instruction =
  | Nop
  | Print of VAR3Expr.expression
  | Exit
  | Write of VAR3Expr.expression * VAR3Expr.expression
  | If    of VAR3Expr.expression * sequence * sequence
  | While of VAR3Expr.expression * sequence
  | Return of VAR3Expr.expression
  | CreateVar of string * VAR3Expr.expression
and sequence = instruction list

let rec sequence_to_string = function
  | [] -> ""
  | i::seq -> (instr_to_string i) ^ (sequence_to_string seq)

and instr_to_string = function
  | Nop -> ""
  | Print(e) -> "print(" ^ (VAR3Expr.to_string e) ^ ");\n"
  | Exit -> "exit;\n"
  | Write(le, e) -> (VAR3Expr.le_to_string le) ^ " := " ^ (VAR3Expr.to_string e) ^ ";\n"
  | If(c, s1, s2) ->
    "if (" ^ (VAR3Expr.to_string c) ^ ") {\n"
    ^ (sequence_to_string s1) ^ "} else {\n"
    ^ (sequence_to_string s2) ^ "}\n"
  | While(c, s) ->
    "while (" ^ (VAR3Expr.to_string c) ^ ") {\n"
    ^ (sequence_to_string s) ^ "}\n"
  | Return(e) ->
    "return (" ^ (VAR3Expr.to_string e) ^ ");\n"
  | CreateVar(lab, value) -> 
    lab ^ " := " ^ (VAR3Expr.to_string value)

and args_to_string = function
  | [] -> ""
  | [a] -> VAR3Expr.to_string a
  | a::args -> (VAR3Expr.to_string a) ^ ", " ^ (args_to_string args)