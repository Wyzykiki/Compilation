{

  open Lexing
  open ARTParser

}

let digit = ['0'-'9']
let number = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' '_'] (letter | digit | '_')*
let commentLine = '#' [^ '\n']* '\n'
  
rule token = parse
  | ['\n']  { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }
  | number as n { INT (n) }     
  | "true"  { BOOL (1) }
  | "false" { BOOL (0) } 
  | ".text" { TEXT }
  | ".data" { DATA }
  | "nop" { NOP }
  | "print" { PRINT }
  | "exit"  { EXIT }
  | "jump"  { JUMP }
  | "when"  { WHEN }
  | ";" { SEMI }
  | ":" { COLON }
  | "(" { LP }
  | ")" { RP }
  | "!" { NEG }
  | "+" { ADD }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { DIV }
  | "%" { REM }
  | "=="  { EQ }
  | "!="  { NEQ }
  | "<" { LT }
  | "<="  { LE }
  | ">" { GT }
  | ">="  { GE }
  | "&&"  { AND }
  | "||"  { OR }
  | ":="  { AFFECT }
  | "&" { AMPERSAND }
  | id as i { ID (i) }
  | commentLine as c  { COMMENT (c) }
  | eof
      { EOF }
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
