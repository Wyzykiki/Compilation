let input_file = Sys.argv.(1)
let _ =
  if not (Filename.check_suffix input_file ".typ") then
    failwith "expected .typ extension"
let input = open_in input_file
let lexing_buffer = Lexing.from_channel input

let prog_typ = TYPParser.program TYPLexer.token lexing_buffer
let _ = TYPCheck.check_program prog_typ
let prog_rec = TYPtoREC.translate_program prog_typ
let prog_tpl = RECtoTPL.translate_program prog_rec
let prog_fnx = TPLtoFNX.translate_program prog_tpl
let prog_seq = FNXtoSEQ.translate_program prog_fnx
let prog_vrx = SEQtoVRX.translate_program prog_seq
let prog_var = VRXtoVAR.translate_program prog_vrx
let _ =
  VAREval.eval_program prog_var
