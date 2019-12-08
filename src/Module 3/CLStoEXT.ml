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
  { EXT.types = List.flatten (List.map tr_clas_def CLS.(prog.classes));
    EXT.text =  List.flatten (List.map tr_methods CLS.(prog.classes)) @ (List.map tr_fdef CLS.(prog.functions));
    EXT.globals = CLS.(prog.globals) }