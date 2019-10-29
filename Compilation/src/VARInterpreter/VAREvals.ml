open VAR
open FUNInstr
open IMPExpr
open Op

(* Environnement global : varibles globales et fonctions du programme *)
type global_env = {
  functions_tbl: (string, function_definition) Hashtbl.t;
  globals_tbl: (string, int) Hashtbl.t
}

(* Environnement local : varibles locales et parametres recus *)
type local_env = {
  locals_tbl: (string, int) Hashtbl.t;
  params_tbl: (string, int) Hashtbl.t
}

exception FunctionResult of int
    
let eval_program prog =
  (* Creer et remplis la table des fonctions  *)
  let functions_tbl = Hashtbl.create 17 in
  List.iter (fun fdef -> Hashtbl.add functions_tbl fdef.name fdef) prog.text;
  
  (* Creer et remplis la tables des variables globales *)
  let globals_tbl = Hashtbl.create 17 in
  List.iter (fun (var, value) -> Hashtbl.add globals_tbl var value) prog.globals;
  
  (* Evalue la fonction a partir de sa definition, des parametres passes et de l'environnement global *)
  let rec eval_function fdef params_ genv =

    (* Creer et remplis la table des varibles locales *)
    let locals_tbl = Hashtbl.create 17 in
    List.iter (fun (var, value) -> Hashtbl.add locals_tbl var value) fdef.locals;
    
    (* Creer et remplis la table des parametres *)
    let params_tbl = Hashtbl.create 17 in
    List.iter2 (fun var value -> Hashtbl.add params_tbl var value) fdef.parameters params_;

    try
      eval_sequence fdef.code { locals_tbl; params_tbl } genv;
      (* Valeur de retour par defaut *)
      None
    with
      | FunctionResult(e) -> Some e
    
  and eval_sequence s lenv genv =
    List.iter (fun i -> eval_instruction i lenv genv) s
      
  and eval_instruction i lenv genv =

    (* Change la valeur de la variable (dans l'environnement où elle est définie) *)
    let replace_in_env var value =
      if Hashtbl.mem lenv.locals_tbl var then Hashtbl.replace lenv.locals_tbl var value
        else  if Hashtbl.mem lenv.params_tbl var then Hashtbl.replace lenv.params_tbl var value
              else  if Hashtbl.mem genv.globals_tbl var then Hashtbl.replace genv.globals_tbl var value
                    else failwith (var ^ " is undefined!")
    in

    match i with
      | Nop -> ()
      | Exit -> exit 0
      | Print(e) ->
        let v = eval_expression e lenv genv in
        Printf.printf "%c" (char_of_int v)
      | Write(Name(id), e) -> 
        replace_in_env id (eval_expression e lenv genv)
      | If(e, s1, s2) -> 
        if (eval_expression e lenv genv) != 0 then eval_sequence s1 lenv genv else eval_sequence s2 lenv genv
      | While(e, s) -> while (eval_expression e lenv genv) != 0 do eval_sequence s lenv genv done
      | Call(Name(address), Name(function_), params_) ->
        let fdef = Hashtbl.find functions_tbl function_ in
        let params = List.map (fun e -> eval_expression e lenv genv) params_ in
        let returned_value = match eval_function fdef params genv with
          | None -> failwith ("Can't assign None to " ^ address)
          | Some(i) -> i
        in
        replace_in_env address returned_value

      | Return(e) -> raise (FunctionResult (eval_expression e lenv genv))
      | _ -> failwith "not implemented"
  
  and eval_expression e lenv genv = match e with
    | Immediate(n) -> n
    | Name(s) -> failwith "An expression can't be a Name."
    | Unop(op, e) -> Op.interpret_unop op (eval_expression e lenv genv)
    | Binop(op, e1, e2) -> Op.interpret_binop op (eval_expression e1 lenv genv) (eval_expression e2 lenv genv)
    | Deref(Name(id)) ->
      if Hashtbl.mem lenv.locals_tbl id then Hashtbl.find lenv.locals_tbl id
      else  if Hashtbl.mem lenv.params_tbl id then Hashtbl.find lenv.params_tbl id
            else  if Hashtbl.mem genv.globals_tbl id then Hashtbl.find genv.globals_tbl id
                  else failwith (id ^ " is undefined!")
    | _ -> failwith "not implemented"

  in

  (* Appelle la fonction main avec ses arguments *)
  let main = Hashtbl.find functions_tbl "main" in
  let param = ref [] in
  for i=Array.length Sys.argv -1 downto 2 do
    param := int_of_string Sys.argv.(i) :: !param
  done;
  eval_function main !param { functions_tbl; globals_tbl }
