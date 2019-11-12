let input_file = Sys.argv.(1)
let _ =
  if not (Filename.check_suffix input_file ".var") then
    failwith "expected .var extension"
let input = open_in input_file
let lexing_buffer = Lexing.from_channel input

let source = VARParser.program VARLexer.token lexing_buffer
  
let target_var2 = VAR3toVAR2.translate_program source
let output_file_var2 = (Filename.chop_suffix input_file ".var") ^ ".var2"
let output_var2 = open_out output_file_var2
let _ = 
  Printf.fprintf output_var2 "%s" (VAR2.prog_to_string target_var2)


let target_var = VAR2toVAR.translate_program target_var2
let output_file_var = (Filename.chop_suffix input_file ".var") ^ ".var1"
let output_var = open_out output_file_var
let _ = 
  Printf.fprintf output_var "%s" (VAR.prog_to_string target_var)


let target_fun = VARtoFUN.translate_program target_var
let output_file_fun = (Filename.chop_suffix input_file ".var") ^ ".fun"
let output_fun = open_out output_file_fun
let _ = 
  Printf.fprintf output_fun "%s" (FUN.prog_to_string target_fun)

let target_cll = FUNtoCLL.translate_program target_fun
let output_file_cll = (Filename.chop_suffix input_file ".var") ^ ".cll"
let output_cll = open_out output_file_cll
let _ = 
  Printf.fprintf output_cll "%s" (CLL.prog_to_string target_cll)

let target_imp = CLLtoIMP.translate_program target_cll
let output_file_imp = (Filename.chop_suffix input_file ".var") ^ ".imp"
let output_imp = open_out output_file_imp
let _ = 
  Printf.fprintf output_imp "%s" (IMP.to_string target_imp)
