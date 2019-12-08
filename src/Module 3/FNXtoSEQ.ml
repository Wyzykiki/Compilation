module FI = FNXInstr
module FE = FNXExpr
module SI = SEQInstr
module SE = IMPExpr

(** 
    Traduction d'une expression avec appels de fonctions en une paire
    formée d'une expression pure et de la séquence des appels de fonction
    à effectuer préalablement à l'évaluation de l'expression.    

    tr_expr: FNXExpr.expression -> IMPExpr.expression * SEQInstr.instruction

    Utilise une fonction auxiliaire pour créer un nouveau nom de variable pour
    stocker le résultat de chaque appel de fonction ainsi extrait.

    new_call_var: unit -> string
*)
let new_call_var =
  let cpt = ref 0 in
  fun () -> incr cpt; Printf.sprintf "_call_var_%i" !cpt

let rec tr_expr = function
  | FE.Immediate(i) -> SE.Immediate(i), SI.Nop
  | FE.Name(id) -> SE.Name(id), SI.Nop
  | FE.Unop(op, e) -> let e, calls = tr_expr e in
                      SE.Unop(op, e), calls
  | FE.Binop(op, e1, e2) -> let e1, calls1 = tr_expr e1 in
                            let e2, calls2 = tr_expr e2 in
                            SE.Binop(op, e1, e2), SI.(calls1 @@ calls2)
  | FE.Deref(e) -> let e, calls = tr_expr e in
                   SE.Deref(e), calls
  | FE.Call(e, args) -> let e, callse = tr_expr e in
                        let args, callsa = List.split(List.map tr_expr args) in
                        let x = new_call_var() in
                        SE.(Deref(Name x)),
                        SI.(callse
                            @@ (List.fold_right (@@) callsa SI.Nop)
                            @@ NewVar(x, SE.(Immediate 0))
                            @@ Call(SE.(Name x), e, args))

(** 
    Traduction des instructions et séquences d'instructions FNX en 
    instructions SEQ. 

    tr_instr: FNXInstr.instruction -> SEQInstr.instruction
    tr_seq: FNXInstr.sequence -> SEQInstr.instruction
*)
let rec tr_instr = function
  | FI.Nop -> SI.Nop
  | FI.Print(e) -> let e, calls = tr_expr e in
                   SI.(calls @@ Print(e))
  | FI.Exit -> Exit
  | FI.Write(d, e) -> let d, callsd = tr_expr d in
                      let e, callse = tr_expr e in
                      SI.(callsd @@ callse @@ Write(d, e))
  | FI.If(e, s1, s2) -> let e, calls = tr_expr e in
                        SI.(calls @@ If(e, tr_seq s1, tr_seq s2))
  | FI.While(e, s) -> let e, calls = tr_expr e in
                      SI.(calls @@ While(e, tr_seq s @@ calls))
  | FI.Return(e) -> let e, calls = tr_expr e in
                    SI.(calls @@ Return(e))
and tr_seq s = SI.(List.fold_right (@@) (List.map tr_instr s) Nop)

(** 
    Traduction des fonctions FNX en fonctions SEQ. Les variables locales
    sont introduites avec le constructeur [NewVar].
*)
let tr_fdef fdef =
  let locals =
    SI.(List.fold_right (@@)
          (List.map (fun (id, v) -> NewVar(id, SE.Immediate v))
             FNX.(fdef.locals))
          Nop)
  in
  { SEQ.name = FNX.(fdef.name);
    SEQ.code = SI.(locals @@ tr_seq FNX.(fdef.code));
    SEQ.parameters = FNX.(fdef.parameters) }

(**
   Traduction des programmes FNX en programmes SEQ.
*)
let translate_program prog =
  { SEQ.text = List.map tr_fdef FNX.(prog.text);
    SEQ.globals = FNX.(prog.globals) }
