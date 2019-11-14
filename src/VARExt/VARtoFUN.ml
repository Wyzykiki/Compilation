module I = IMPExpr
open FUNInstr

(* Remplace les références à des variables locales par les calculs
   d'adresse *)
let rec translate_expression expr alloc_table = match expr with
  | I.Immediate(i) -> I.Immediate(i)
  | I.Unop(op, e) -> I.Unop(op, translate_expression e alloc_table)
  | I.Binop(op, e1, e2) -> I.Binop(op, translate_expression e1 alloc_table, translate_expression e2 alloc_table)
  | I.Deref(e) -> I.Deref(translate_expression e alloc_table)
  | I.Name(id) ->
    if Hashtbl.mem alloc_table id then I.sub (I.Deref(I.Name("frame_pointer"))) (I.Immediate(Hashtbl.find alloc_table id))
    else I.Name(id)

(* Instructions et séquences : traduction iso *)
let rec translate_instruction instr alloc_table = match instr with
  | Nop -> Nop  
  | Print(e) -> Print(translate_expression e alloc_table)
  | Exit -> Exit
    
  | Write(e1, e2) ->
    Write(translate_expression e1 alloc_table,
          translate_expression e2 alloc_table)
      
  | If(c, s1, s2) ->
    If(translate_expression c alloc_table,
       translate_sequence s1 alloc_table,
       translate_sequence s2 alloc_table)
  | While(c, s) ->
    While(translate_expression c alloc_table,
          translate_sequence s alloc_table)
    
  | Call(d, f, args) ->
    Call(translate_expression d alloc_table,
         translate_expression f alloc_table,
         List.map (fun a -> translate_expression a alloc_table) args)
      
  | Return(e) ->
    Return(translate_expression e alloc_table)

and translate_sequence seq alloc_table =
  List.map (fun i -> translate_instruction i alloc_table) seq

(* Ajoute au code habituel l'initialisation des variables locales *)
let translate_function_definition fdef =
  (* On créer une table pour chaque fonction qui associe le nom des variables locales avec leurs adresses relative à FP *)
  let alloc_table = Hashtbl.create 17 in
  (* Liste des instructions d'empilement des variables locales *)
  let locals_decl = 
    List.mapi (fun index (var, value) ->
      Hashtbl.add alloc_table var (1+index);
      push (I.Immediate(value))
    )
    VAR.(fdef.locals);
  in
  
  { FUN.name = VAR.(fdef.name);
    FUN.code = List.flatten locals_decl @ translate_sequence VAR.(fdef.code) alloc_table;
    FUN.parameters = VAR.(fdef.parameters) }

let translate_program prog =
  { FUN.text = List.map translate_function_definition VAR.(prog.text);
    FUN.data = VAR.(prog.globals) }
