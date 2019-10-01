%{

  open Lexing

%}

(* File structure tokens *)
%token EOF
%token TEXT DATA
%token SEMI COLON
%token LP RP

%token <string>INT
%token <int>BOOL
%token <string>ID
%token <string>COMMENT

(* Unary operators/instructions *)

%token NOP EXIT PRINT NEG JUMP

(* Binary operators/instructions *)

%token ADD MINUS STAR DIV REM EQ NEQ LT LE GT GE AND OR WHEN AFFECT

(* Priority and associativity rules *)

%left ADD MINUS OR
%left STAR DIV REM AND
%right NEG
%nonassoc EQ NEQ LT LE GT GE

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
| label=ID COLON is=instructions { label ^ ":\n" ^ is }
| c=COMMENT is=instructions { c ^ is }
;
  
instruction:
| NOP	{ "NOP" }
| EXIT { "EXIT" }
| PRINT LP e=expression RP { e ^ " PRINT" }
| JUMP e=l_expr	{ e ^ " JUMP" }
| JUMP e1=l_expr WHEN e2=expression	{ e1 ^ " " ^ e2 ^ " JUMPWHEN" }
| e1=l_expr	AFFECT e2=expression { e1 ^ " " ^ e2 ^ " WRITE" }
;

expression:
| i=INT  { i }
| b=BOOL { string_of_int b }
| e=l_expr	{ e  ^ " READ" }
| LP e=expression RP { e }
| op=unop	e=expression { e ^ " " ^ op }
| e1=expression op=binop e2=expression	{ e1 ^ " " ^ e2 ^ " " ^ op }
;

%inline l_expr:
| i=ID	{ i }
| STAR e=expression	{ e ^ " READ" }
;

%inline unop:
| MINUS	{ "MINUS" }
| NEG	{ "NEG" }
;

%inline binop:
| ADD { "ADD" }
| MINUS { "SUB" }
| STAR { "MULT" }
| DIV { "DIV" }
| REM { "REM" }
| EQ { "EQ" }
| NEQ { "NEQ" }
| LT { "LT" }
| LE { "LE" }
| GT { "GT" }
| GE { "GE" }
| AND { "AND" }
| OR { "OR" }
;

data_declarations:
| (* empty *) { "" }
| label=ID COLON i=INT data=data_declarations { label ^ ": " ^ i ^ "\n" ^ data }
| c=COMMENT data=data_declarations { c ^ data }
;
