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
    
let rec type_to_string = function
  | TInt -> "int"
  | TBool -> "bool"
  | TPointer(ty) -> Printf.sprintf "%s*" (type_to_string ty)
  | TArray(ty) -> Printf.sprintf "(%s)[]" (type_to_string ty)
  | TFun(ty, ps) -> ""
  | TRecord(lab) -> Printf.sprintf "struct %s" lab

let () =
  Printexc.register_printer
    (function
      | IncompatibleTypes(expected, actual) -> Some (Printf.sprintf "IncompatibleTypes : %s was expected but got %s instead." (type_to_string expected) (type_to_string actual))
      | UnknownIdentifier(id) -> Some (Printf.sprintf "UnknownIdentifier \"%s\"" id)
      | PointerTypeExpected -> Some ("PointerTypeExpected")
      | ArrayTypeExpected -> Some ("ArrayTypeExpected")
      | RecordTypeExpected -> Some ("RecordTypeExpected")
      | FunctionTypeExpected -> Some ("FunctionTypeExpected")
      | UnknownField(record, field) -> Some (Printf.sprintf "UnknownField %s for the record %s" field record) 
      | _ -> None (* for other exceptions *)
    )

type global_env = {
  globals: (string, expr_type) Hashtbl.t;
  functions: (string, (expr_type * expr_type list)) Hashtbl.t;
  records: (string, (string * expr_type) list) Hashtbl.t
}

type local_env = {
  locals: (string, expr_type) Hashtbl.t;
  parameters: (string, expr_type) Hashtbl.t;
  ret_type: expr_type
}


let rec type_expr genv lenv e = match e with
  | Immediate(n) -> if (n>1 || n<0) then TInt else TBool
  | Name(id) -> 
    if Hashtbl.mem lenv.locals id then Hashtbl.find lenv.locals id
    else if Hashtbl.mem lenv.parameters id then Hashtbl.find lenv.parameters id
    else if Hashtbl.mem genv.globals id then Hashtbl.find genv.globals id
    else raise (UnknownIdentifier(id))
  
  | Deref(le) -> TPointer(type_expr genv lenv le)

  | Unop(Minus, e) ->let typeUnop = type_expr genv lenv e in
    if typeUnop = TInt || typeUnop = TBool then TInt else raise (IncompatibleTypes(TInt, typeUnop))
  
  | Unop(Not, e) ->let typeUnop = type_expr genv lenv e in
    if typeUnop = TBool then TBool else raise (IncompatibleTypes(TBool, typeUnop))
  
  | Binop((Add | Sub | Mult | Div | Rem), e1, e2) -> let type1 = type_expr genv lenv e1 in
    let type2 = type_expr genv lenv e2 in
    if (type1 = TInt || type1 = TBool) then 
      begin if (type2 = TInt || type2 = TBool) then TInt else raise (IncompatibleTypes(TInt, type2)) end
    else raise (IncompatibleTypes(TInt, type1))
  
  | Binop((Or | And), e1, e2) -> let type1 = type_expr genv lenv e1 in
    let type2 = type_expr genv lenv e2 in
    if type1 = TBool then 
      begin if type2 = TBool then TBool else raise (IncompatibleTypes(TBool, type2)) end
    else raise (IncompatibleTypes(TBool, type1))

  | Binop((Lt | Le | Gt | Ge), e1, e2) ->let type1 = type_expr genv lenv e1 in
    let type2 = type_expr genv lenv e2 in
    if (type1 = TInt || type1 = TBool) then 
      begin if (type2 = TInt || type2 = TBool) then TBool else raise (IncompatibleTypes(TInt, type2)) end
    else raise (IncompatibleTypes(TInt, type1))

  | Binop((Eq | Neq), e1, e2) -> let type1 = type_expr genv lenv e1 in
    let type2 = type_expr genv lenv e2 in
    if type1 = type2 then TBool else raise (IncompatibleTypes(type1, type2))

  | Call(Name(f), args) -> 
    if Hashtbl.mem genv.functions f
    then begin List.iter2 (fun arg ty_attendu -> let ty_arg = type_expr genv lenv arg in if ty_arg != ty_attendu then raise (IncompatibleTypes(ty_attendu, ty_arg))) args (snd (Hashtbl.find genv.functions f));
      (fst (Hashtbl.find genv.functions f)) end
    else raise (UnknownIdentifier(f))

  | ArrayAccess(Name(a), i) -> let ty_i = type_expr genv lenv i in
    if ty_i = TInt || ty_i = TBool then
    begin
        (* Variable locale *)
        if Hashtbl.mem lenv.locals a then
        begin
          let arr = Hashtbl.find lenv.locals a in
          match arr with
          | TArray(ty_array) -> ty_array
          | _ -> raise (ArrayTypeExpected)
        (* Paramètre *)
        end 
        else begin 
          if Hashtbl.mem lenv.parameters a then
          begin
            let arr = Hashtbl.find lenv.parameters a in
            match arr with
            | TArray(ty_array) -> ty_array
            | _ -> raise (ArrayTypeExpected)
            (* Variable globale *)
          end 
          else begin
            if Hashtbl.mem genv.globals a then
            begin
                let arr = Hashtbl.find genv.globals a in
                match arr with
                | TArray(ty_array) -> ty_array
                | _ -> raise (ArrayTypeExpected)
            end
            else raise (UnknownIdentifier(a))
          end
        end
      end
      else raise (IncompatibleTypes(TInt, ty_i))

  | RecAccess(record, field) -> 
    let ty_r = type_expr genv lenv record in
    begin match ty_r with
      | TPointer(TRecord(lab)) -> 
        if Hashtbl.mem genv.records lab then
        begin
          if List.exists (fun f -> field = fst f ) (Hashtbl.find genv.records lab) then
            snd (List.find (fun f -> field = fst f ) (Hashtbl.find genv.records lab))
          else raise (UnknownField(lab, field))
        end
        else raise (UnknownIdentifier(lab))
      | TPointer(_) -> raise (RecordTypeExpected)
      | _ -> raise (PointerTypeExpected)
    end


  | _ -> failwith ""(* TODO: on peut enlever? *)

let rec check_instr genv lenv i = match i with
  | Nop -> ()
  | Print(e) -> ()
  | Exit -> ()

  | Write(le, e) -> let attendu = type_expr genv lenv le in
    let ptr_fourni = type_expr genv lenv e in
    begin match ptr_fourni with
      | TPointer(fourni) -> if (attendu = TInt && fourni != TBool && fourni != TInt) || (fourni != attendu && attendu != TInt) then raise (IncompatibleTypes(attendu, fourni))
      | fourni -> if (attendu = TInt && fourni != TBool && fourni != TInt) || (fourni != attendu && attendu != TInt) then raise (IncompatibleTypes(attendu, fourni))
      (* | _ -> () *)
    end

  | If(c, s1, s2) -> let ty_c = type_expr genv lenv c in
    if ty_c != TBool then raise (IncompatibleTypes(TBool, ty_c))
    else check_sequence genv lenv s1;
      check_sequence genv lenv s2

  | While(c, s) -> let ty_c = type_expr genv lenv c in
    if ty_c != TBool then raise (IncompatibleTypes(TBool, ty_c)) 
    else
    check_sequence genv lenv s

  | Return(e) -> let attendu = lenv.ret_type in 
    let fourni = type_expr genv lenv e in
    if fourni != attendu then raise (IncompatibleTypes(attendu, fourni))

  | MkArray(le, taille, e) -> let ty_remplissage = type_expr genv lenv e in
    let ty_var = type_expr genv lenv le in
    begin match ty_var with
      | TPointer(TArray(ty_r)) -> if ty_remplissage != ty_r && (ty_remplissage = TInt && ty_r != TBool) then raise (IncompatibleTypes(ty_r, ty_remplissage))
      | _ -> raise (ArrayTypeExpected)
    end

  | NewRec(le, lab) -> let ty_var = type_expr genv lenv le in
    if ty_var != TPointer(TRecord(lab)) then raise (RecordTypeExpected)

and check_sequence genv lenv s =
  List.iter (check_instr genv lenv) s
  

let check_fdef genv fdef = 
  (* On créer une table associant les variables à leur types *)
  let locals = Hashtbl.create 17 in
  List.iter (fun (lab, ty, _) -> Hashtbl.add locals lab ty) TYP.(fdef.locals);

  (* On créer une table assocaint les paramètres à leur types *)
  let parameters = Hashtbl.create 17 in
  List.iter (fun (lab, ty) -> Hashtbl.add parameters lab ty) fdef.parameters;

  check_sequence genv { locals; parameters; ret_type=(fdef.ret_type) } TYP.(fdef.code)


let check_program prog =
  (* On créer une table associant les variables à leur types *)
  let globals = Hashtbl.create 17 in
  List.iter (fun (lab, ty, _) -> Hashtbl.add globals lab ty) TYP.(prog.globals);
  
  (* On créer une table associant les fonctions à leur type de retour et leurs paramètres *)
  let functions = Hashtbl.create 17 in
  List.iter (fun fdef -> 
              let get_param_types params = List.map (fun (_, ty) -> ty) params in  
              Hashtbl.add functions fdef.name (fdef.ret_type, (get_param_types fdef.parameters))
            ) TYP.(prog.text);
  
  (* On sauvegarde les types struct du programme *)
  let records = Hashtbl.create 17 in
  List.iter (fun rdef -> Hashtbl.add records rdef.rec_name rdef.fields) TYP.(prog.types);

  List.iter (check_fdef { globals; functions; records }) TYP.(prog.text)