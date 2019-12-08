%{

  open Lexing
  open CLS
  open RECInstr
  open RECExpr
  open Op

%}

(* Base *)
%token NOP PRINT EXIT
%token SEMI COMMA
%token SET
%token <int>INT
%token <bool>BOOL
%token <string>LABEL
%token EOF
(* Arithmétique *)
%token PLUS MINUS STAR SLASH PRCT
%token EQ NEQ LT LE GT GE
%token AND OR NOT
%token LP RP
(* Blocs / fonctions *)
%token BEGIN END
%token IF ELSE
%token WHILE
%token RETURN
(* Variables *)
%token VAR
(* Struct *)
%token (* STRUCT *) DOT LB RB NEW
(* Classes *)
%token CLASS EXTENDS

%left AND OR
%left LT LE GT GE EQ NEQ
%left PLUS MINUS
%left STAR SLASH PRCT
%nonassoc NOT

%start program
%type <CLS.program> program

%%

program:
| classes=list(class_declaration)
  globals=list(data_declaration)
  functions=list(function_definition) EOF
    { { classes; functions; globals } }
| error { let pos = $startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message }
;

extension:
| EXTENDS parent=LABEL { parent }
;
    
class_declaration:
| CLASS class_name=LABEL
    ext=option(extension)
    BEGIN
    attributes=list(data_declaration)
    methods=list(function_definition)
    END
    {
      let attributes = List.map fst attributes in
      let parent = match ext with
        | None -> "_master"
        | Some p -> p
      in
      { class_name; attributes; methods; parent }
    }
;

data_declaration:
| VAR lab=LABEL SEMI { lab, 0 }
| VAR lab=LABEL SET i=immediate SEMI { lab, i }
;

function_definition:
| name=LABEL
    LP parameters=separated_list(COMMA, LABEL) RP
    BEGIN locals=list(data_declaration) code=list(terminated_instruction) END
    { { name; code; parameters; locals } }
;

block:
| BEGIN seq=list(terminated_instruction) END { seq }
;

terminated_instruction:
| i=instruction SEMI { i }
| IF LP e=expression RP s1=block ELSE s2=block { If(e, s1, s2) }
| WHILE LP e=expression RP s=block { While(e, s) }
;

instruction:
| NOP { Nop }
| PRINT LP e=expression RP { Print(e) }
| EXIT { Exit }
| le=left_expression SET e=expression { Write(le, e) }
| RETURN LP e=expression RP { Return(e) }
| le=left_expression SET LP e=expression RP LB s=expression RB
    { MkArray(le, s, e) }
| le=left_expression SET NEW lab=LABEL { NewRec(le, lab) }
;

expression:
| i=immediate { Immediate(i) }
| le=left_expression { Deref(le) }
| LP e=expression RP { e }
| uop=unop e=expression { Unop(uop, e) }
| e1=expression bop=binop e2=expression { Binop(bop, e1, e2) }
| f=left_expression LP args=separated_list(COMMA, expression) RP
    { Call(f, args) }
;

left_expression:
| l=LABEL { Name(l) }
| STAR LP e=expression RP { e }
| a=left_expression LB i=expression RB { ArrayAccess(a, i) }
| e=left_expression DOT lab=LABEL { RecAccess(e, lab) }
;

immediate:
| i=INT  { i }
| b=BOOL { if b then 1 else 0 }
;

%inline unop:
| MINUS { Minus }
| NOT   { Not   }
;

%inline binop:
| PLUS  { Add  }
| MINUS { Sub  }
| STAR  { Mult }
| SLASH { Div  }
| PRCT  { Rem  }
| EQ    { Eq   }
| NEQ   { Neq  }
| LT    { Lt   }
| LE    { Le   }
| GT    { Gt   }
| GE    { Ge   }
| AND   { And  }
| OR    { Or   }
;
