module O = OTFInstr
module V = FUNInstr

(* Instructions : traduction iso *)
let rec translate_instruction = function
  | O.Nop -> V.Nop
  | O.Exit -> V.Exit
  | O.Write(le, e) -> V.Write(le, e)
  | O.If(c, s1, s2) -> V.If(c, translate_sequence s1, translate_sequence s2)
  | O.While(c, s) -> V.While(c, translate_sequence s)
  | O.Call(d, f, args) -> V.Call(d, f, args)
  | O.Return(e) -> V.Return(e)
  | O.CreateVar(lab, value) -> V.Write(IMPExpr.Name(lab), value)

and translate_sequence s =
  List.map translate_instruction s

let translate_function_definition fdef =
  (* Cherche si des variables locales sont définis et les ajoute à la liste pour VAR *)
  let rec retrieve_locals = function
    | O.CreateVar(lab, value) -> [ (lab, 0) ]
    | O.If(c, s1, s2) -> List.flatten (List.map retrieve_locals s1 @ List.map retrieve_locals s2)
    | O.While(c, s) -> List.flatten (List.map retrieve_locals s)
    | _ -> []
  in

  { VAR.name = OTF.(fdef.name);
    VAR.code = translate_sequence OTF.(fdef.code);
    VAR.parameters = OTF.(fdef.parameters);
    VAR.locals = List.flatten (List.map retrieve_locals OTF.(fdef.code)) }

let translate_program prog =
  { VAR.text = List.map translate_function_definition OTF.(prog.text);
    VAR.globals = OTF.(prog.globals) }