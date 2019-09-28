%{

  open Lexing

%}

%token TEXT DATA
%token PRINT EXIT
%token SEMI
%token <int>INT
%token EOF
%token LP RP
/* TODO: les tokens */%token ADD
%start program
%type <string> program

%%

program:
| TEXT text=instructions DATA data=data_declarations EOF
    { ".text\n" ^ text ^ ".data\n" ^ data }
| error { let pos = $startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message }
;

instructions:
| (* empty *) { "" }
| i=instruction SEMI is=instructions { i ^ "\n" ^ is }
;
  
instruction:
/* | NOP	{ "NOP" } */
| EXIT { "EXIT" }
| PRINT LP e=expression RP { e ^ " PRINT" }
/* | JUMP e=l_expr	{  }
| JUMP e1=l_expr WHEN e2=expression	{  }
| e=l_expr	{  } */
;

expression:
| i=INT  { string_of_int i }
/* | b=BOOL {  }
| e=l_expr	{ e }
| LP e=expression RP {  }
| op=unop	{  }
| e1=expression op=BINOP e2=expression	{ op } */
;

/* l_expr:
| i=ID	{ i }
| STAR e=expression	{ "READ" ^ e }

unop:
| MINUS	{ "MINUS" }
| NEG	{ "NEG" }

%inline binop:
| ADD { "ADD" }
| MINUS { "MINUS" }
| STAR { "MULT" }
| DIV { "DIV" }
| REM { "REM" }
| EQ { "EQ" }
| NEQ { "NEQ" }
| LT { "LT" }
| LE { "LE" }
|  { "" }
|  { "" }
|  { "" }
|  { "" }
|  { "" }
|  { "" }
|  { "" }
|  { "" } */

data_declarations:
| (* empty *) { "" }
;
