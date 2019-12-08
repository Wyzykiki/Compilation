(* On créer une table qui associe à chaque nom d'enregistrement, le nom de ses champs. *)
let assoc_tbl = Hashtbl.create 17

(* Traduction des records EXT vers REC *)
let tr_rec_def rdef =
  let fields = 
    match EXT.(rdef.parent) with
      | None -> EXT.(rdef.fields)
      | Some(parent) -> let parent_fields = (Hashtbl.find assoc_tbl parent) in
      parent_fields @ List.flatten (List.map (fun f -> if (List.mem f parent_fields) then [] else [f]) EXT.(rdef.fields))
  in
  Hashtbl.add assoc_tbl EXT.(rdef.rec_name) fields;

  { REC.rec_name = EXT.(rdef.rec_name);
    REC.fields = fields }

let tr_fdef fdef =
  { REC.name = EXT.(fdef.name);
    REC.code = EXT.(fdef.code);
    REC.parameters = EXT.(fdef.parameters);
    REC.locals = EXT.(fdef.locals) }

let translate_program prog =
  { REC.types = List.map tr_rec_def EXT.(prog.types);
    REC.text = List.map tr_fdef EXT.(prog.text);
    REC.globals = EXT.(prog.globals) }
