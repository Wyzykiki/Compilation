open RECInstr 
open RECExpr

let tr_fdef fdef = 
  { EXT.name = CLS.(fdef.name);
    EXT.code = CLS.(fdef.code);
    EXT.parameters = CLS.(fdef.parameters);
    EXT.locals = CLS.(fdef.locals) }

let tr_methods cdef = 
  let tr_method fdef= 
    { EXT.name = CLS.(fdef.name);
      EXT.code = (* changer les appels de methode ?! *)CLS.(fdef.code);
      EXT.parameters = "this" :: CLS.(fdef.parameters);
      EXT.locals = CLS.(fdef.locals) }
  in
  List.map tr_method CLS.(cdef.methods)

let rec tr_expr = function
  | Immediate(e) -> Immediate(e)
  (* appartient à param alors rec acces sinon on laisse comme ça*)
  (* Expression this (?) alors que string*)
  | Name(s) -> if (* Accès à la table *) then RecAccess(Name("this"), s) else Name(s)
  | Unop(op, e) -> Unop(op, tr_expr e)
  | Binop(op, e1, e2) -> Binop(op, tr_expr e1, tr_expr e2)
  | Deref(e) -> Deref(tr_expr e)
    (* *)
  | Call(e, list_e) -> Call(tr_expr e, List.map(fun x -> tr_expr x) list_e)
  | ArrayAccess(base, indice) -> ArrayAccess(tr_expr base, tr_expr indice)
  | RecAccess(base, champ) -> RecAccess(tr_expr base, champ)

let rec tr_instr = function 
  |  Nop -> Nop
  |  Print(e) -> Print(tr_expr e)
  |  Exit -> Exit
  |  Write(e1, e2) -> Write(tr_expr e1, tr_expr e2)
  |  If(c, s1, s2) -> If(tr_expr c, tr_seq s1, tr_seq s2)
  |  While(c, s) -> While(tr_expr c, tr_seq s)
  |  Return(e) -> Return(tr_expr e)
  |  MkArray(adresse, taille, elem) -> MkArray(tr_expr adresse, tr_expr taille, tr_expr elem)
  |  NewRec(adresse, nom) -> NewRec(tr_expr adresse, nom) 
and tr_seq seq = List.map (fun x -> tr_instr x) seq

let tr_clas_def cdef =
  let parent = if CLS.(cdef.parent) = "_master" then None else Some(CLS.(cdef.parent)) in

  (* Structure pour les descripteurs de classes. *)
  [{  EXT.parent = parent;
      EXT.rec_name = CLS.(cdef.class_name) ^ "_descr";
      EXT.fields = CLS.(cdef.parent) :: (List.map (fun fdef -> CLS.(cdef.class_name) ^ "_" ^ CLS.(fdef.name)) CLS.(cdef.methods)) };
  (* Structure pour les instances de classes. *)
  { EXT.parent = None;
    EXT.rec_name = CLS.(cdef.class_name);
    EXT.fields = (CLS.(cdef.class_name) ^ "_descr") :: CLS.(cdef.attributes) }
  ]

let translate_program prog =
  let classes = Hashtbl.create 17 in 
  List.iter (fun x -> Hashtbl.add classes (fst CLS.(x.class_name)) (snd CLS.(x))) CLS.(prog.classes);

  let globals = Hashtbl.create 17 in 
  List.iter (fun x -> Hashtbl.add globals (fst CLS.(x.globals)) (snd CLS.(x.globals))) CLS.(prog.globals);

  (* Il faut passer en parametre locals et globals *)
  { EXT.types = {parent=None; rec_name="_master"; fields=[]} :: List.flatten (List.map tr_clas_def CLS.(prog.classes));
    EXT.text =  List.flatten (List.map tr_methods CLS.(prog.classes)) @ (List.map tr_fdef CLS.(prog.functions));
    EXT.globals = CLS.(prog.globals) }