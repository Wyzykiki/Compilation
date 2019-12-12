module RI = RECInstr
module RE = RECExpr
module TI = TPLInstr
module TE = TPLExpr

(* Création d'un tuple à partir d'un tableau *)
let mk_array =
  { 
    TPL.name = "mk_array";
    (* Adresse -> la deuxième case ? *)
    TPL.code = [
                TI.MkTuple(TE.Name("ret"), TE.Binop(Op.Add,TE.Deref(TE.Name("taille")), TE.Immediate(1)));
                TI.Write(TE.Call(TE.Name "array_get", [TE.Deref(TE.Name("ret")); TE.Immediate(0)]), TE.Deref(TE.Name "taille"));
                TI.While (TE.Binop (Op.Le, TE.Deref(TE.Name "i"), TE.Deref(TE.Name "taille")),
                    [TI.Write(TE.Call(TE.Name "array_get", [TE.Deref(TE.Name("ret")); TE.Deref(TE.Name "i")]), TE.Deref(TE.Name "elem"));
                    TI.Write(TE.Name "i", TE.Binop(Op.Add, TE.Deref(TE.Name "i"), TE.Immediate(1)))]);
                TI.Return(TE.Binop(Op.Add,TE.Deref(TE.Name("ret")), TE.Immediate(1)))
              ];
    TPL.parameters = ["taille"; "elem"] ;
    TPL.locals = [("i", 1); ("ret", 0)];
  }

(* Accès à une case du tableau*)
let array_get = 
  { 
    TPL.name = "array_get" ;
    TPL.code = [
                TI.Return(TE.TupleAccess(TE.Deref(TE.Name "adresse"), TE.Deref(TE.Name "indice")))
    ];
    TPL.parameters = ["adresse"; "indice"];
    TPL.locals = [];
  }

let rec tr_expr records_def records_instance = function
  | RE.Immediate(e) -> TE.Immediate(e)
  | RE.Name(s) -> TE.Name(s)
  | RE.Unop(op, e) -> TE.Unop(op, tr_expr records_def records_instance e)
  | RE.Binop(op, e1, e2) -> TE.Binop(op, tr_expr records_def records_instance e1, tr_expr records_def records_instance e2)
  | RE.Deref(e) -> TE.Deref(tr_expr records_def records_instance e)
  | RE.Call(e, list_e) -> TE.Call(tr_expr records_def records_instance e, List.map(fun x -> tr_expr records_def records_instance x) list_e)
  | RE.ArrayAccess(base, indice) -> TE.TupleAccess(tr_expr records_def records_instance base, TE.Binop(Op.Add, tr_expr records_def records_instance indice, TE.Immediate(1))) (* Warning sur l'addition*)
  | RE.RecAccess(base, champ) -> let rec_name = Hashtbl.find records_instance base in
    let listeDesChamps = Hashtbl.find records_def rec_name in 
    let placeDuChamp = ref 0 in
    List.iteri (fun i x -> if x = champ then placeDuChamp := i) listeDesChamps; 
    TE.TupleAccess(TE.Deref(tr_expr records_def records_instance base), TE.Binop(Op.Add, TE.Immediate(!placeDuChamp), TE.Immediate(1)) )

let rec tr_instr records_def records_instance = function 
  | RI.Nop -> TI.Nop
  | RI.Print(e) -> TI.Print(tr_expr records_def records_instance e)
  | RI.Exit -> TI.Exit
  | RI.Write(e1, e2) -> TI.Write(tr_expr records_def records_instance e1, tr_expr records_def records_instance e2)
  | RI.If(c, s1, s2) -> TI.If(tr_expr records_def records_instance c, tr_seq s1 records_def records_instance, tr_seq s2 records_def records_instance)
  | RI.While(c, s) -> TI.While(tr_expr records_def records_instance c, tr_seq s records_def records_instance)
  | RI.Return(e) -> TI.Return(tr_expr records_def records_instance e)
  | RI.MkArray(adresse, taille, elem) -> TI.Write(tr_expr records_def records_instance adresse, TE.Call(TE.Name "mk_array", [tr_expr records_def records_instance taille; tr_expr records_def records_instance elem ])) (* Traduction faite dans mk_array*)
  | RI.NewRec(adresse, nom) -> Hashtbl.add records_instance adresse nom;
    let taille = List.length (Hashtbl.find records_def nom) in
    TI.Write(tr_expr records_def records_instance adresse, TE.Call(TE.Name "mk_array", [tr_expr records_def records_instance (RE.Immediate(taille)); tr_expr records_def records_instance (RE.Immediate(-467294563)) ]))
and tr_seq seq records_def records_instance = List.map (fun x -> tr_instr records_def records_instance x) seq

let tr_fdef fdef records_def records_instance =
  {
    TPL.name = REC.(fdef.name);
    TPL.code = tr_seq REC.(fdef.code) records_def records_instance;
    TPL.parameters = REC.(fdef.parameters);
    TPL.locals = REC.(fdef.locals)
  }

let translate_program prog =
  (* On créé une table qui associe le nom d'un type enregistrement avec ses champs *)
  let records_def = Hashtbl.create 17 in 
  List.iter (fun x -> Hashtbl.add records_def REC.(x.rec_name) REC.(x.fields)) REC.(prog.types);
  (* On créé une table pour les instances des enregistrements associant le nom de la variable avec son type d'enregistrement *)
  let records_instance = Hashtbl.create 17 in
  {
    TPL.text =  array_get :: mk_array ::  (List.map (fun fdef -> tr_fdef fdef records_def records_instance) REC.(prog.text));
    TPL.globals = REC.(prog.globals)

  }