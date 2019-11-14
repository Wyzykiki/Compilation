module FI = FEXInstr
module OI = OTFInstr
module FE = FEXExpr
module IE = IMPExpr

(* Créer une variable pour pouvoir sauvegarder le résultat d'une fonction. *)
let new_var =
  let cpt = ref 0 in
  fun () -> cpt := !cpt+1; Printf.sprintf "call_in_expr_var_%i" !cpt

(* Traduit les expressions et dans le cas d'un appel de fonction, founit les instructions nécessaires à son évaluation. *)
let rec translate_expression = function
  | FE.Immediate(i) -> IE.Immediate(i), []
  | FE.Name(id) -> IE.Name(id), []
  | FE.Unop(op, e) -> let expr, seq = translate_expression e in
      IE.Unop(op, expr), seq

  | FE.Binop(op, e1, e2) -> let expr1, seq1 = translate_expression e1 in
      let expr2, seq2 = translate_expression e2 in
      IE.Binop(op, expr1, expr2), seq1 @ seq2

  | FE.Deref(e) -> let expr, seq = translate_expression e in
      IE.Deref(expr), seq

  | FE.Call(f, args) -> let var = new_var () in
      let expr1, seq1 = translate_expression f in
      let expr2, seq2 = List.split (List.map translate_expression args) in
      IE.Deref(IE.Name(var)), seq1 @ List.flatten seq2 @ [ OI.CreateVar(var, IE.Immediate(0)); OI.Call(IE.Name(var), expr1, expr2) ]

(* Traduction des instructions et ajoute les instructions supplémentaires s'il y a un appel de fonction dans une expression. *)
let rec translate_instruction = function
  | FI.Nop -> [ OI.Nop ]

  | FI.Exit -> [ OI.Exit ]
  | FI.Write(le, e) -> let le2, calls = translate_expression le in
      let expr, calls2 = translate_expression e in
      calls @ calls2 @ [ OI.Write(le2, expr) ]

  | FI.If(c, s1, s2) -> let expr, calls = translate_expression c in
      calls @ [ OI.If(expr, translate_sequence s1, translate_sequence s2) ]

  (* Si la condition est un appel de fonction, il faut penser à réévaluer l'appel à chaque fois dans la boucle. *)
  | FI.While(c, s) -> let expr, calls = translate_expression c in
      calls @ [ OI.While(expr, [ List.hd (List.rev calls) ] @ translate_sequence s) ]

  | FI.Return(e) -> let expr, calls = translate_expression e in
      calls @ [ OI.Return(expr) ]

  | FI.CreateVar(lab, value) -> let expr, calls = translate_expression value in
      calls @ [ OI.CreateVar(lab, expr) ]


and translate_sequence s =
  List.flatten (List.map translate_instruction s)

let translate_function_definition fdef =
  { OTF.name = FEX.(fdef.name);
    OTF.code = translate_sequence FEX.(fdef.code);
    OTF.parameters = FEX.(fdef.parameters); }

let translate_program prog =
  { OTF.text = List.map translate_function_definition FEX.(prog.text);
    OTF.globals = FEX.(prog.globals) }