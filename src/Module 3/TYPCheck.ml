open TYP
open RECInstr
open RECExpr
open Op

exception IncompatibleTypes of expr_type (* expected *) * expr_type (* actual *)
exception UnknownIdentifier of string
exception PointerTypeExpected
exception ArrayTypeExpected
exception RecordTypeExpected
exception FunctionTypeExpected
exception UnknownField of string (* record *) * string (* field *)
    
type global_env = {
  globals: (string, expr_type) Hashtbl.t;
  functions: (string, expr_type list) Hashtbl.t
}

type local_env = {
  locals: (string, expr_type) Hashtbl.t;
  parameters: (string, expr_type) Hashtbl.t;
  ret_type: expr_type
}




let type_expr genv lenv e = match e with
  (* | Immediate(n) -> ... (* si n pas 0 ou 1, TInt, mais sinon peut-être TBool *)
  | Name(id) -> TPointer((* aller consulter les tables des variables *))
  | Binop((Add | Sub), e1, e2) -> (* vérifier (type_expr e1 = TInt) et (type_expr e2 = TInt) *)
  Tint
  | Binop(Or, e1, e2) -> (* vérifier (type_expr e1 = TBool) et (type_expr e2 = TBool) *)
  TBool

  | Call(f, args) *)
  | _ -> failwith ""

let rec check_instr genv lenv i = match i with
  | Nop -> ()
  | Print(e) -> ()
  | Exit -> ()

  | Write(le, e) -> let attendu = type_expr genv lenv le in(*FIXME:faut mettre des TPointer ???*)
    let fourni = type_expr genv lenv e in
    if fourni != attendu then raise (IncompatibleTypes(attendu, fourni))

  | If(c, s1, s2) -> let ty_c = type_expr genv lenv c in
    if ty_c != TBool then raise (IncompatibleTypes(TBool, ty_c))
    check_sequence genv lenv s1;
    check_sequence genv lenv s2

  | While(c, s) -> let ty_c = type_expr genv lenv c in
    if ty_c != TBool then raise (IncompatibleTypes(TBool, ty_c))
    check_sequence genv lenv s

  | Return(e) -> let attendu = lenv.ret_type in 
    let fourni = type_expr genv lenv e in
    if fourni != attendu then raise (IncompatibleTypes(attendu, fourni))

  | MkArray(le, taille, e) -> let ty_remplissage = type_expr genv lenv e in(*FIXME:faut mettre des TPointer ???*)
    let ty_var = type_expr genv lenv le in
    if ty_var != TArray(ty_r) then raise (ArrayTypeExpected) else
      if ty_remplissage != ty_r then raise (IncompatibleTypes(ty_r, ty_remplissage))

  | NewRec(le, lab) -> let ty_var = type_expr genv lenv le in(*FIXME:faut mettre des TPointer ???*)
    if ty_var != TRecord(lab) then raise (RecordTypeExpected)

and check_sequence genv lenv s =
  List.iter (check_instr genv lenv) s

(* let check_rdef rdef = *)
  

let check_fdef genv fdef = 
  (* On créer une table associant les variables à leur types *)
  let locals = Hashtbl.create 17 in
  List.iter (fun (lab, ty, _) -> Hashtbl.add locals lab ty) TYP.(fdef.locals);

  (* On créer une table assocaint les paramètres à leur types *)
  let parameters = Hashtbl.create 17 in
  List.iter (fun (lab, ty) -> Hashtbl.add parameters lab ty) fdef.parameters;

  check_sequence genv { locals; parameters; ret_type=(fdef.ret_type) } TYP.(fdef.code)
  (* Verif ret-type *)
  (* if fdef.ret_type != let (ty, _) = Hashtbl.find genv.functions fdef.name in ty
  then raise IncompatibleTypes(fdef.ret_type, ty) *)

  (* Renvoie le type de la variable *)
(* let type_of_var env var =
  Hashtbl.find_opt env var *)

let check_program prog =
  (* On créer une table associant les variables à leur types *)
  let globals = Hashtbl.create 17 in
  List.iter (fun (lab, ty, _) -> Hashtbl.add globals lab ty) TYP.(prog.globals);
  
  (* On créer une table associant les fonctions à //leur type de retour et \\leurs paramètres 
  ***********************************pour les appels de fct?*****************************************
  *)
  let functions = Hashtbl.create 17 in
  List.iter (fun fdef -> 
              let get_param_types params = List.map (fun (_, ty) -> ty) params in  
              Hashtbl.add functions fdef.name (get_param_types fdef.parameters)
            ) TYP.(prog.text);
  (* List.iter check_rdef TYP.(prog.types)  On sauvegarde les types struct du programme *)

  List.iter (check_fdef { globals; functions }) TYP.(prog.text)