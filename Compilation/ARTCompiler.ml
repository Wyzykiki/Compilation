(* Ouverture du fichier source et création du buffer d'analyse *)
let input_file = Sys.argv.(1)
let _ =
  if not (Filename.check_suffix input_file ".art") then
    failwith "expected .art extension"
let input = open_in input_file
let lexing_buffer = Lexing.from_channel input

(* Ouverture du fichier cible *)
let output_file = (Filename.chop_suffix input_file ".art") ^ ".stk"
let output = open_out output_file

(* Lecture, analyse, traduction et écriture de la chaîne obtenue dans
   le fichier cible *)
let _ =
  Printf.fprintf output "%s" (ARTParser.program ARTLexer.token lexing_buffer)
