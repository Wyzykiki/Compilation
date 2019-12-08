let input_file = Sys.argv.(1)
let _ =
  if not (Filename.check_suffix input_file ".cls") then
    failwith "expected .cls extension"
let input = open_in input_file
let lexing_buffer = Lexing.from_channel input

let prog_cls = CLSParser.program CLSLexer.token lexing_buffer
let _ = print_string "parsing ok\n"
let prog_ext = CLStoEXT.translate_program prog_cls
let _ = print_string "CLS -> EXT ok\n"
let prog_rec = EXTtoREC.translate_program prog_ext
let _ = print_string "EXT -> REC ok\n"
let prog_tpl = RECtoTPL.translate_program prog_rec
let _ = print_string "REC -> TPL ok\n"
let prog_fnx = TPLtoFNX.translate_program prog_tpl
let _ = print_string "TPL -> FNX ok\n"
let prog_seq = FNXtoSEQ.translate_program prog_fnx
let _ = print_string "FNX -> SEQ ok\n"
let prog_vrx = SEQtoVRX.translate_program prog_seq
let _ = print_string "SEQ -> VRX ok\n"
let prog_var = VRXtoVAR.translate_program prog_vrx
let _ = print_string "VRX -> VAR ok\n"
  
let output_file = (Filename.chop_suffix input_file ".cls") ^ ".var"
let output = open_out output_file
(* let _ = 
  Printf.fprintf output "%s" (VAR.prog_to_string prog_var) *)

let _ =
  VAREvalFP.eval_program prog_var
