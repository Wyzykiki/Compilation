let tr_rec_def rdef =
  { REC.rec_name = TYP.(rdef.rec_name);
    REC.fields = List.map fst TYP.(rdef.fields); }

let tr_fun_def fdef =
  { REC.name = TYP.(fdef.name);
    REC.code = TYP.(fdef.code);
    REC.parameters = List.map fst TYP.(fdef.parameters);
    REC.locals = List.map (fun (id, _, v) -> (id, v)) TYP.(fdef.locals) }

let translate_program prog =
  { REC.types = List.map tr_rec_def TYP.(prog.types);
    REC.text = List.map tr_fun_def TYP.(prog.text);
    REC.globals = List.map (fun (id, _, v) -> (id, v)) TYP.(prog.globals) }
