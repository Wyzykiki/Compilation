module V2 = VAR2Instr
module V = FUNInstr

let rec translate_instruction = function
  | V2.Nop -> [ V.Nop ]
  | V2.Print(e) -> [ V.Print(e) ]
  | V2.Exit -> [ V.Exit ]
  | V2.Write(le, e) -> [ V.Write(le, e) ]
  | V2.If(c, s1, s2) -> [ V.If(c, translate_sequence s1, translate_sequence s2) ]
  | V2.While(c, s) ->  [ V.While(c, translate_sequence s) ]
  | V2.Call(d, f, args) -> [ V.Call(d, f, args) ]
  | V2.Return(e) -> [ V.Return(e) ]
  | V2.CreateVar(lab, value) -> [ V.Write(IMPExpr.Name(lab), value) ]

and translate_sequence s =
  List.flatten (List.map translate_instruction s)

let translate_function_definition fdef =
  let rec retrieve_locals = function
    | V2.CreateVar(lab, value) -> [ (lab, 0) ]
    | V2.If(c, s1, s2) -> List.flatten (List.map retrieve_locals s1 @ List.map retrieve_locals s2)
    | V2.While(c, s) -> List.flatten (List.map retrieve_locals s)
    | _ -> []
  in

  { VAR.name = VAR2.(fdef.name);
    VAR.code = translate_sequence VAR2.(fdef.code);
    VAR.parameters = VAR2.(fdef.parameters);
    VAR.locals = List.flatten (List.map retrieve_locals VAR2.(fdef.code)) }

let translate_program prog =
  { VAR.text = List.map translate_function_definition VAR2.(prog.text);
    VAR.globals = VAR2.(prog.globals) }