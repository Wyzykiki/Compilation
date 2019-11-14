module I = IMPExpr
open FUNInstr

(* Assure que chaque valeur de paramètre est correcte (déréférence ceux passés par référence) *)
let rec translate_expression expr param_table = match expr with
  | I.Immediate(i) -> I.Immediate(i)
  | I.Unop(op, e) -> I.Unop(op, translate_expression e param_table)
  | I.Binop(op, e1, e2) -> I.Binop(op, translate_expression e1 param_table, translate_expression e2 param_table)
  | I.Deref(e) -> I.Deref(translate_expression e param_table)
  | I.Name(id) -> if Hashtbl.mem param_table id then if Hashtbl.find param_table id then I.Deref(I.Name(id)) else I.Name(id) else I.Name(id)

(* Instructions et séquences : traduction iso *)
let rec translate_instruction param_table instr = match instr with
  | Nop -> Nop
  | Exit -> Exit
    
  | Write(e1, e2) ->
    Write(translate_expression e1 param_table,
          translate_expression e2 param_table)
      
  | If(c, s1, s2) ->
    If(translate_expression c param_table,
      translate_sequence s1 param_table,
      translate_sequence s2 param_table)
  | While(c, s) ->
    While(translate_expression c param_table,
          translate_sequence s param_table)
    
  | Call(d, f, args) ->
    Call(translate_expression d param_table,
        translate_expression f param_table,
        List.map (fun a -> translate_expression a param_table) args)
      
  | Return(e) ->
    Return(translate_expression e param_table)

and translate_sequence s param_table =
  List.map (translate_instruction param_table) s

let translate_function_definition fdef = 
  (* On créer une table par fonction, associant le nom des paramètres et s'ils sont passés par référence ou par valeur *)
  let param_table = Hashtbl.create 17 in
  List.iter (fun (lab, is_ref) -> Hashtbl.add param_table lab is_ref) REF.(fdef.parameters);

  { VAR.name = REF.(fdef.name);
    VAR.code = translate_sequence REF.(fdef.code) param_table;
    VAR.parameters = List.map (fun (lab, _) -> lab) REF.(fdef.parameters);
    VAR.locals = REF.(fdef.locals) }

let translate_program prog =
  { VAR.text = List.map translate_function_definition REF.(prog.text);
    VAR.globals = REF.(prog.globals) }