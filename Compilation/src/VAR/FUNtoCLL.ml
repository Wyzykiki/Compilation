module F = FUNInstr
module C = CLLInstr

(* Dans les expressions, remplace les paramètres formels d'une fonction par 
   le calcul d'adresse correspondant. *)
let rec translate_expression expr param_table =
  failwith "not implemented"


(* Instructions ne faisant pas référence aux fonctions : traduction iso *)
(* Pour Call et Return, appliquer le protocole *)
let rec translate_instruction instr param_table =
  match instr with
    | F.Nop -> [ C.Nop ]
    | F.Exit -> [ C.Exit ]
    | F.Print(e) -> [ C.Print(e) ]
    | F.Write(le, e) -> [ C.Write(le, e) ]
    | F.If(c, s1 ,s2) -> [ C.If(c, translate_sequence s1 param_table, translate_sequence s2 param_table) ]
    | F.While(c, s) -> [ C.While(c, translate_sequence s param_table) ]
    | F.Call(d, f, args) -> failwith "not implemented" (* C.Write(d, Name("function_return")) *)
    | F.Return(e) ->  failwith "not implemented"

and translate_sequence s param_table =
  List.flatten (List.map translate_instruction s param_table)

let translate_function_definition fdef =
  { CLL.name = FUN.(fdef.name);
    CLL.code = translate_sequence FUN.(fdef.code) }

    
let translate_program prog =
  { CLL.text = List.flatten (List.map translate_function_definition FUN.(prog.text));
    CLL.data = FUN.(prog.data) }
