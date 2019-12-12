module TI = TPLInstr
module TE = TPLExpr
module FI = FNXInstr
module FE = FNXExpr

let rec translate_expression expr= 
 match expr with
 |TE.Immediate(i)-> FE.Immediate(i)
 |TE.Name(e)-> FE.Name(e)
 |TE.Unop(op,e1)-> FE.Unop(op,(translate_expression e1))
 |TE.Binop(op,e1,e2)-> FE.Binop(op,(translate_expression e1),(translate_expression e2))
 |TE.Deref(e)-> FE.Deref(translate_expression e)
 |TE.Call(e,elist)-> FE.Call((translate_expression e), List.map (fun x-> (translate_expression x))elist )
 |TE.TupleAccess(e,dec)-> FE.Binop(Op.Add ,(translate_expression e),(translate_expression dec))

let rec tr_ins inst=
 match inst with 
 |TI.Nop -> FI.Nop
 |TI.Print(e)-> FI.Print((translate_expression e))
 |TI.Exit-> FI.Exit
 |TI.Write(e1,e2)-> FI.Write((translate_expression e1),(translate_expression e2))
 |TI.If(e,s1,s2)-> FI.If((translate_expression e),(tr_seq s1),(tr_seq s2))
 |TI.While(c, s) -> FI.While((translate_expression c), tr_seq s)
 |TI.Return(e)-> FI.Return((translate_expression e))
 |TI.MkTuple(e1,e2)-> FI.Write((translate_expression e1),FE.Call(FE.Name"malloc",[translate_expression e2]))

and tr_seq seq = List.map (fun x -> tr_ins x) seq


let tr_fdef fdef =
 { FNX.name = TPL.(fdef.name);
 FNX.code = tr_seq TPL.(fdef.code);
 FNX.parameters =TPL.(fdef.parameters);
 FNX.locals = TPL.(fdef.locals) }
 
 
(* Version rudimentaire : ne vérifie pas que la mémoire reste dans une
 certaine plage. *)
 
 
 let malloc =
 { FNX.name = "malloc";
 FNX.code =
 FI.(FE.([ Write(Name "a", Deref(Name "memory_break"));
 Write(Name "memory_break", Binop(Op.Add,
 Deref(Name "memory_break"),
 Deref(Name "n")));
 Return(Deref(Name "a")) ]));
 FNX.parameters = ["n"];
 FNX.locals = [("a",0)] }
 
let translate_program prog =
 { FNX.text = malloc :: List.map tr_fdef TPL.(prog.text);
 FNX.globals = ("memory_break", 16384) :: TPL.(prog.globals) }