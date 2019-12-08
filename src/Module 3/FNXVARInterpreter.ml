let input_file = Sys.argv.(1)
let _ =
  if not (Filename.check_suffix input_file ".fnx") then
    failwith "expected .fnx extension"
let input = open_in input_file
let lexing_buffer = Lexing.from_channel input

let prog_fnx = FNXParser.program FNXLexer.token lexing_buffer
let prog_seq = FNXtoSEQ.translate_program prog_fnx
let prog_vrx = SEQtoVRX.translate_program prog_seq
let prog_var = VRXtoVAR.translate_program prog_vrx
let _ =
  VAREval.eval_program prog_var
