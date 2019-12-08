module SI = SEQInstr
module VI = VRXInstr

(** 
    Traduction des instructions SEQ en séquences d'instructions VRX.
    Formule utilisant un accumulateur pour éviter de coûteuses concaténations
    de listes.

    tr_instr: SEQInstr.instruction -> VRXInstr.sequence -> VRXInstr.sequence

    Le résultat de [tr_instr i s] est équivalent à la concaténation de la
    traduction de [i] avec [s].
*)
let rec tr_instr instr seq = match instr with
  | SI.Nop -> VI.Nop :: seq
  | SI.Print(e) -> VI.Print(e) :: seq
  | SI.Exit -> VI.Exit :: seq
  | SI.Write(d, e) -> VI.Write(d, e) :: seq
  | SI.If(e, i1, i2) -> VI.If(e, tr_instr i1 [], tr_instr i2 []) :: seq
  | SI.While(e, i) -> VI.While(e, tr_instr i []) :: seq
  | SI.Return(e) -> VI.Return(e) :: seq
  | SI.Call(d, f, args) -> VI.Call(d, f, args) :: seq
  | SI.NewVar(id, e) -> VI.NewVar(id, e) :: seq
  | SI.Seq(i1, i2) -> tr_instr i1 (tr_instr i2 seq)

let tr_fdef fdef =
  { VRX.name = SEQ.(fdef.name);
    VRX.code = tr_instr SEQ.(fdef.code) [];
    VRX.parameters = SEQ.(fdef.parameters) }

let translate_program prog =
  { VRX.text = List.map tr_fdef SEQ.(prog.text);
    VRX.globals = SEQ.(prog.globals) }

(** 
    Pour info, la version inefficace de [tr_instr] aurait pu être :

    let rec tr_instr = function
      | Nop -> [ Nop ]
      | Print(e) -> [ Print(e) ]
      | Exit -> [ Exit ]
      | Write(d, e) -> [ Write(d, e) ]
      | If(e, i1, i2) -> [ If(e, tr_instr i1, tr_instr i2) ]
      | While(e, i) -> [ While(e, tr_instr i) ]
      | Return(e) -> [ Return(e) ]
      | Call(d, f, args) -> [ Call(d, f, args) ]
      | NewVar(id, e) -> [ NewVar(id, e) ]
      | Seq(i1, i2) -> (tr_instr i1) @ (tr_instr i2)
*)
