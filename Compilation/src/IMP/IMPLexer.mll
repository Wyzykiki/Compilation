{

  open Lexing
  open IMPParser
  
}

let digit = ['0'-'9']
let number = digit+
let letter = ['a'-'z' 'A'-'Z']
let label = ['a'-'z' '_'] (letter | digit | '_')*
  
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
  | "goto"
      { GOTO }
  | "while"
      { WHILE }
  | "for"
      { FOR }
  | "if"
      { IF }
  | "else"
      { ELSE }
  | "break"
      { BREAK }
  | "continue"
      { CONTINUE }
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