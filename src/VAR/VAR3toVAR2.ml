module V3I = VAR3Instr
module V2I = VAR2Instr
module VE = VAR3Expr
module IE = IMPExpr

let new_var =
  let cpt = ref 0 in
  fun () -> cpt := !cpt+1; Printf.sprintf "call_in_expr_var_%i" !cpt

let rec translate_expression = function
  | VE.Immediate(i) -> IE.Immediate(i), []
  | VE.Name(id) -> IE.Name(id), []
  | VE.Unop(op, e) -> let expr, seq = translate_expression e in
      IE.Unop(op, expr), seq

  | VE.Binop(op, e1, e2) -> let expr1, seq1 = translate_expression e1 in
      let expr2, seq2 = translate_expression e2 in
      IE.Binop(op, expr1, expr2), seq1 @ seq2

  | VE.Deref(e) -> let expr, seq = translate_expression e in
      IE.Deref(expr), seq

  | VE.Call(f, args) -> let var = new_var () in
      let expr1, seq1 = translate_expression f in
      let expr2, seq2 = List.split (List.map translate_expression args) in
      IE.Deref(IE.Name(var)), seq1 @ List.flatten seq2 @ [ V2I.CreateVar(var, IE.Immediate(0)); V2I.Call(IE.Name(var), expr1, expr2) ]

let rec translate_instruction = function
  | V3I.Nop -> [ V2I.Nop ]
  | V3I.Print(e) -> let expr, calls = translate_expression e in
      calls @ [ V2I.Print(expr) ]

  | V3I.Exit -> [ V2I.Exit ]
  | V3I.Write(le, e) -> let le2, calls = translate_expression le in
      let expr, calls2 = translate_expression e in
      calls @ calls2 @ [ V2I.Write(le2, expr) ]

  | V3I.If(c, s1, s2) -> let expr, calls = translate_expression c in
      calls @ [ V2I.If(expr, translate_sequence s1, translate_sequence s2) ]

  | V3I.While(c, s) -> let expr, calls = translate_expression c in
      calls @ [ V2I.While(expr, translate_sequence s) ]

  (* | Call(d, f, args) -> Call(d, f, args) *)
  | V3I.Return(e) -> let expr, calls = translate_expression e in
      calls @ [ V2I.Return(expr) ]

  | V3I.CreateVar(lab, value) -> let expr, calls = translate_expression value in
      calls @ [ V2I.CreateVar(lab, expr) ]


and translate_sequence s =
  List.flatten (List.map translate_instruction s)

let translate_function_definition fdef =
  { VAR2.name = VAR3.(fdef.name);
    VAR2.code = translate_sequence VAR3.(fdef.code);
    VAR2.parameters = VAR3.(fdef.parameters); }

let translate_program prog =
  { VAR2.text = List.map translate_function_definition VAR3.(prog.text);
    VAR2.globals = VAR3.(prog.globals) }