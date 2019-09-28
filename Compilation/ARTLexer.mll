{

  open Lexing
  open ARTParser

}

let digit = ['0'-'9']
let number = digit+
  
rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | number as n
      { INT (int_of_string n) }      
  | ".text" { TEXT }
  | ".data"
      { DATA }
  | "print"
      { PRINT }
  | "exit"
      { EXIT }
  | ";"
      { SEMI }
  | "("
      { LP }
  | ")"
      { RP }
  | "+" {ADD}
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof
      { EOF }
