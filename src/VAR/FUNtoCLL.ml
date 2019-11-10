module F = FUNInstr
module C = CLLInstr
module I = IMPExpr

(* Dans les expressions, remplace les paramètres formels d'une fonction par 
   le calcul d'adresse correspondant. *)
let rec translate_expression expr param_table = match expr with
  | I.Immediate(i) -> I.Immediate(i)
  | I.Unop(op, e) -> I.Unop(op, translate_expression e param_table)
  | I.Binop(op, e1, e2) -> I.Binop(op, translate_expression e1 param_table, translate_expression e2 param_table)
  | I.Deref(e) -> I.Deref(translate_expression e param_table)
  | I.Name(id) ->
    if Hashtbl.mem param_table id then I.add (I.Deref(I.Name("frame_pointer"))) (I.Immediate(Hashtbl.find param_table id))
    else I.Name(id)

(* Instructions ne faisant pas référence aux fonctions : traduction iso *)
(* Pour Call et Return, appliquer le protocole *)
let rec translate_instruction param_table instr = match instr with
  | F.Nop -> [ C.Nop ]
  | F.Exit -> [ C.Exit ]
  | F.Print(e) -> [ C.Print(translate_expression e param_table) ]
  | F.Write(le, e) -> [ C.Write(translate_expression le param_table, translate_expression e param_table) ]
  | F.If(c, s1 ,s2) -> [ C.If(translate_expression c param_table, translate_sequence s1 param_table, translate_sequence s2 param_table) ]
  | F.While(c, s) -> [ C.While(translate_expression c param_table, translate_sequence s param_table) ]
  | F.Call(d, f, args) -> 
      (* Protocole étape 1 *)
      List.flatten (List.map C.push (List.map (fun e -> translate_expression e param_table) args))
      @ [ C.Call(f) ]
      (* Protocole étape 5 *)
      @ List.flatten (List.map C.pop args)
      @ [ C.Write(translate_expression d param_table, I.Deref(I.Name("function_return"))) ]
  (* Protocole étape 4 *)
  | F.Return(e) -> [ C.Write(I.Name("function_return"), translate_expression e param_table); C.Return ]

and translate_sequence s param_table =
  List.flatten (List.map (translate_instruction param_table) s)

let translate_function_definition fdef =
  (* On créer une table pour chaque fonction qui associe le nom des paramètres avec leurs adresses relative à FP *)
  let param_table = Hashtbl.create 17 in
  let n = List.length FUN.(fdef.parameters) in
  List.iteri (fun index p_name -> Hashtbl.add param_table p_name (1+n-index)) FUN.(fdef.parameters);

  { CLL.name = FUN.(fdef.name);
    CLL.code = translate_sequence FUN.(fdef.code) param_table }
    
let translate_program prog =
  { CLL.text = List.map translate_function_definition FUN.(prog.text);
    CLL.data = FUN.(prog.data) @ [ ("function_return", 0) ] }
