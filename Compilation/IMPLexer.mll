{

  open Lexing
  open IMPParser
  
}

let digit = ['0'-'9']
let number = digit+
let label = ['a'-'z' '_']+
  
rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | number as n
      { INT(int_of_string n) }
  | ".text"
      { TEXT }
  | ".data"
      { DATA }
  | "nop"
      { NOP }
  | "print"
      { PRINT }
  | "exit"
      { EXIT }
  | "while"
      { WHILE }
  | "if"
      { IF }
  | "else"
      { ELSE }
  | "true"
      { BOOL("true") }
  | "false"
      { BOOL("false") }
  | label as lab
      { LABEL(lab) }
  | "!"
      { NEG }
  | ";"
      { SEMI }
  | ":"
      { COLON }
  | "("
      { LP }
  | ")"
      { RP }
  | "{"
      { LB }
  | "}"
      { RB }
  | "+"
      { ADD }
  | "-"
      { MINUS }
  | "*"
      { STAR }
  | "/"
      { DIV }
  | "%"
      { REM }
  | "==" 
      { EQ }
  | "!=" 
      { NEQ }
  | "<"
      { LT }
  | "<=" 
      { LE }
  | ">"
      { GT }
  | ">=" 
      { GE }
  | "&&" 
      { AND }
  | "||" 
      { OR }
  | ":=" 
      { AFFECT }
  | eof
      { EOF }
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
