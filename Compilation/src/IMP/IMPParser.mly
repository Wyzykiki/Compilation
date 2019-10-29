%{

  open Lexing

%}

(* File structure tokens *)
%token EOF
%token TEXT DATA
%token SEMI COLON
%token LP RP LB RB

%token <int>INT
%token <string>LABEL
%token <string>BOOL

(* Unary operators/instructions *)

%token NOP EXIT PRINT NEG

(* Binary operators/instructions *)

%token ADD MINUS STAR DIV REM EQ NEQ LT LE GT GE AND OR AFFECT

(* If/Loop tokens *)
%token IF ELSE WHILE GOTO FOR BREAK CONTINUE

(* Priority and associativity rules *)

%left ADD MINUS OR
%left STAR DIV REM AND
%right NEG
%nonassoc EQ NEQ LT LE GT GE

%start program
%type <IMP.program> program

%%

program:
| TEXT text=list(instruction) DATA data=list(data_declaration) EOF
    { { IMP.text=text; IMP.data=data } }
| error { let pos = $startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message }
;

instruction:
| l=LABEL COLON { IMPInstr.Label(l) }
| NOP SEMI { IMPInstr.Nop }
| PRINT LP e=expression RP SEMI { IMPInstr.Print(e) }
| EXIT SEMI                     { IMPInstr.Exit }
| IF LP e=expression RP LB then_i=list(instruction) RB ELSE LB else_i=list(instruction) RB  { IMPInstr.If(e, then_i, else_i) }
| WHILE LP e=expression RP LB do_i=list(instruction) RB  { IMPInstr.While(e, do_i) }
| GOTO LP e=l_expr RP SEMI  { IMPInstr.Goto(e) }
| a=affect SEMI	{ a }
| FOR LP e=expression SEMI step=affect RP LB do_i=list(instruction) RB { IMPInstr.While(e, do_i@[step]) }
| BREAK SEMI	{ IMPInstr.Break }
| CONTINUE SEMI	{ IMPInstr.Continue }
;

affect:
| e1=l_expr	AFFECT e2=expression { IMPInstr.Write(e1, e2) }
;

expression:
| i=INT { IMPExpr.Immediate(i) }
| b=BOOL { IMPExpr.Name(b) }
| e=l_expr	{ IMPExpr.Deref(e) }
| LP e=expression RP { e }
| op=unop	e=expression { IMPExpr.Unop(op, e) }
| e1=expression op=binop e2=expression	{ IMPExpr.Binop(op, e1, e2) }
;

%inline l_expr:
| i=LABEL	{ IMPExpr.Name(i) }
| STAR e=expression	{ e }
;

%inline unop:
| MINUS	{ Op.Minus }
| NEG	{ Op.Not }
;

%inline binop:
| ADD { Op.Add }
| MINUS { Op.Sub }
| STAR { Op.Mult }
| DIV { Op.Div }
| REM { Op.Rem }
| EQ { Op.Eq }
| NEQ { Op.Neq }
| LT { Op.Lt }
| LE { Op.Le }
| GT { Op.Gt }
| GE { Op.Ge }
| AND { Op.And }
| OR { Op.Or }
;

data_declaration:
| l=LABEL COLON v=INT { l, v }
;