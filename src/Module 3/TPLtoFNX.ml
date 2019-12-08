module TI = TPLInstr
module TE = TPLExpr
module FI = FNXInstr
module FE = FNXExpr

let tr_fdef fdef =
  failwith "not implemented"
  
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
