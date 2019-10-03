{
	(* Prélude : ouverture des fichiers et définition de fonctions auxiliaires *)

	open Lexing

	(* Exception à déclencher à la fin du fichier *)
	exception Eof

	(* Récupération du nom du fichier source sur la ligne de commande *)
	let input_file = Sys.argv.(1)
	let _ =
		if not (Filename.check_suffix input_file ".stk") then
			failwith "expected .stk extension"

	(* Ouverture du fichier source et création du fichier cible *)
	let input = open_in input_file
	let output = open_out ((Filename.chop_suffix input_file ".stk") ^ ".asm")

	(* Fonction d'écriture spécialisée pour cibler le fichier cible *)
	let print s = Printf.fprintf output "%s" s

	(* Création d'une structure d'analyse lexicale [lexbuf] initialisée avec le texte du fichier source *)
	let lexbuf = from_channel input

	(* Prend la valeur dans le registre r et la place au sommet de la pile *)
	let push r = 
		print "  DIRECTREAD $r15 stack_pointer\n";
		print "  DECR $r15 1\n";
		print ("  WRITE $r15 " ^ r ^ "\n");
		print "  DIRECTWRITE stack_pointer $r15\n"
		
	(* Prend la valeur au sommet de la pile et la place dans le registre r *)
	let pop r = 
		print "  DIRECTREAD $r15 stack_pointer\n";
		print ("  READ " ^ r ^ " $r15\n");
		print "  INCR $r15 1\n";
		print "  DIRECTWRITE stack_pointer $r15\n"

	(* Chaque instruction indique si son action sur l'acc doit être sauvegarde dans le cas d'un remplacement. *)
	let saveAcc = ref false

	(* Le pointeur de registre de pile *)
	let register_sp = ref 0

	let push_reg r =
		if !register_sp > 11 then push ("$r" ^ string_of_int (!register_sp mod 12));
		print ("  MOVE $r" ^ string_of_int (!register_sp mod 12) ^ " " ^ r ^ "\n");
		register_sp := !register_sp + 1

	let pop_reg r =
		register_sp := !register_sp - 1;
		print ("  MOVE " ^ r ^ " $r" ^ string_of_int (!register_sp mod 12) ^ "\n");
		if !register_sp > 11 then pop ("$r" ^ string_of_int (!register_sp mod 12))
}

let spaces = ['\n' ' ']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id = (letter | digit | '_')+
let commentLine = '#' [^ '\n']* '\n'

(* Les regles dans la partie .text *)
rule instruction = parse
	| ".text"	{ print "#Instruction\n";
							instruction lexbuf
						}
	| ".data"	{	print "#Donnee\n";
							donnee lexbuf
						}
	| "NOP"	{ print "  NOP\n";
						instruction lexbuf
					}
	| "EXIT" 	{ print "  EXIT\n";
							instruction lexbuf
						}
	| "PRINT"	{ print "  PRINT $r12\n";
							saveAcc := false;
							instruction lexbuf
						}
	| "READ"	{ print "  READ $r12 $r12\n";
							saveAcc := true;
							instruction lexbuf
						}
	| "WRITE"	{ pop_reg "$r13";
							print "  WRITE $r13 $r12\n";
							saveAcc := false;
							instruction lexbuf
						}
	| "JUMP"	{ print "  JUMP $r12\n";
							saveAcc := false;
							instruction lexbuf
						}
	| "JUMPWHEN"	{ pop_reg "$r13";
									print "  JUMP $r13 WHEN $r12\n";
									saveAcc := false;
									instruction lexbuf
								}
	| "MINUS"	{ print "  MINUS $r12 $r12\n";
							saveAcc := true;
							instruction lexbuf
						}
	| "NOT"	{ print "  NEG $r12 $r12\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "ADD"	{ pop_reg "$r13";
						print "  ADD $r12 $r13 $r12\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "SUB"	{ pop_reg "$r13";
						print "  SUB $r12 $r13 $r12\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "MULT"	{ pop_reg "$r13";
							print "  MULT $r12 $r13 $r12\n";
							saveAcc := true;
							instruction lexbuf
						}
	| "DIV"	{ pop_reg "$r13";
						print "  DIV $r12 $r13 $r12\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "REM"	{ pop_reg "$r13";
						print "  REM $r12 $r13 $r12\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "EQ"	{ pop_reg "$r13";
						print "  EQ $r12 $r13 $r12\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "NEQ"	{ pop_reg "$r13";
						print "  NEQ $r12 $r13 $r12\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "LT"	{ pop_reg "$r13";
						print "  LT $r12 $r13 $r12\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "LE"	{ pop_reg "$r13";
						print "  LE $r12 $r13 $r12\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "GT"	{ pop_reg "$r13";
						print "  GT $r12 $r13 $r12\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "GE"	{ pop_reg "$r13";
						print "  GE $r12 $r13 $r12\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "AND"	{ pop_reg "$r13";
						print "  AND $r12 $r13 $r12\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "OR"	{ pop_reg "$r13";
						print "  OR $r12 $r13 $r12\n";
						saveAcc := true;
						instruction lexbuf
					}
	| digit+	{	if !saveAcc then push_reg "$r12"; (* On sauvegarde ce qu'il y avait dans l'acc *)
              print ("  CONST $r12 " ^ (lexeme lexbuf) ^ "\n");
							saveAcc := true;
							instruction lexbuf
						}
	| id	{ if !saveAcc then push_reg "$r12"; (* On sauvegarde ce qu'il y avait dans l'acc *)
          print ("  ADDRESS $r12 " ^ (lexeme lexbuf) ^ "\n");
					saveAcc := true;
					instruction lexbuf
				}
	| id ':'	{ print ((lexeme lexbuf) ^ "\n");
							instruction lexbuf
						}
	| spaces	{ instruction lexbuf }
	| commentLine	{ print (lexeme lexbuf);
									instruction lexbuf
								}
	| _	{ failwith ("Unknow character : " ^ (lexeme lexbuf)) }

(* Les regles dans la partie .data *)
and donnee = parse
	| id ':'	{ print ((lexeme lexbuf) ^ "\n");
							donnee lexbuf
						}
	| digit+	{ print ("  " ^ (lexeme lexbuf) ^ "\n");
							donnee lexbuf
						}
	| eof	{ raise Eof }
	| spaces { donnee lexbuf }
	| commentLine	{ print (lexeme lexbuf);
									donnee lexbuf
								}
	| _	{ failwith ("Unknow character : " ^ (lexeme lexbuf)) }

{
	(* Main *)
	let _ =
		try
			instruction lexbuf
		with Eof -> (* fin du programme, fermer le fichier cible *)
			print "stack_pointer:\n";
			print "  65536";
			close_in input;
			close_out output
}