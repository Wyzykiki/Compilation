module S = IMP
module I = IMPInstr
module T = ART

let new_label =
  let cpt = ref 0 in
  fun () -> incr cpt; Printf.sprintf "label_%i" !cpt

let rec translate_instruction = function
      
  | I.Nop -> [ T.Nop ]

  | I.Print(e) ->
    [ T.Print(e) ]
      
  | I.Exit ->
    [ T.Exit ]
  
  | I.If(e, s1, s2) -> let l_then = new_label () in
    let next = new_label () in
    List.flatten [[ T.JumpWhen(IMPExpr.Name(l_then), e) ]; translate_sequence s2;
    [ T.Jump(IMPExpr.Name(next)); T.Label(l_then) ]; translate_sequence s1; [ T.Label(next) ]]

  | I.While(e, s) -> let test = new_label () in
    let loop = new_label () in
    List.flatten [[ T.Jump(IMPExpr.Name(test)); T.Label(loop) ]; translate_sequence s;
    [ T.Label(test); T.JumpWhen(IMPExpr.Name(loop), e) ]]

  | I.Label(s) -> [ T.Label(s) ]

  | I.Goto(e) -> [ T.Jump(e) ]

  | I.Write(le, e) -> [ T.Write(le, e) ]

      
and translate_sequence s =
  List.flatten (List.map translate_instruction s)

let translate_program p =
  { T.text = translate_sequence S.(p.text);
    T.data = S.(p.data) }
