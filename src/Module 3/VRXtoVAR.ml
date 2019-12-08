module XI = VRXInstr
module VI = FUNInstr

(**
   Traduction des instructions VRX en instructions VAR.
   La seule modification concerne l'instruction [NewVar], remplacée
   par une instruction d'écriture affectant à la variable la valeur
   prescrite.
*)
let rec tr_instr = function
  | XI.Nop -> VI.Nop
  | XI.Print(e) -> VI.Print(e)
  | XI.Exit -> VI.Exit
  | XI.Write(d, e) -> VI.Write(d, e)
  | XI.If(e, s1, s2) -> VI.If(e, tr_seq s1, tr_seq s2)
  | XI.While(e, s) -> VI.While(e, tr_seq s)
  | XI.Call(d, f, args) -> VI.Call(d, f, args)
  | XI.Return(e) -> VI.Return(e)
  | XI.NewVar(id, e) -> VI.Write(Name id, e)
and tr_seq s =
  List.map tr_instr s

(**
   Collecte des noms des variables déclarées dans une séquence d'instructions.
*)
let rec collect_vars_instr = function
  | XI.NewVar(id, e) -> [ id ]
  | _ -> []
and collect_vars_seq s =
  List.flatten(List.map collect_vars_instr s)

(**
   Traduction des fonctions VRX en fonctions VAR.
   L'ensemble des variables locales est calculé en collectant les variables
   déclarées dans le corps de la fonction.
*)
let tr_fdef fdef =
  { VAR.name = VRX.(fdef.name);
    VAR.code = tr_seq VRX.(fdef.code);
    VAR.parameters = VRX.(fdef.parameters);
    VAR.locals =
      List.map (fun id -> (id, 0)) (collect_vars_seq VRX.(fdef.code)) }

(**
   Traduction des programmes VRX en programmes VAR.
*)
let translate_program prog =
  { VAR.text = List.map tr_fdef VRX.(prog.text);
    VAR.globals = VRX.(prog.globals) }
