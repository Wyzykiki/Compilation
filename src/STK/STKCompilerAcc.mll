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
}

let spaces = ['\n' ' ']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id = ['a'-'z' '_'] (letter | digit | '_')*
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
	| "PRINT"	{ print "  PRINT $r0\n";
							saveAcc := false;
							instruction lexbuf
						}
	| "READ"	{ print "  READ $r0 $r0\n";
							saveAcc := true;
							instruction lexbuf
						}
	| "WRITE"	{ pop "$r1";
							print "  WRITE $r1 $r0\n";
							saveAcc := false;
							instruction lexbuf
						}
	| "JUMP"	{ print "  JUMP $r0\n";
							saveAcc := false;
							instruction lexbuf
						}
	| "JUMPWHEN"	{ pop "$r1";
									print "  JUMP $r1 WHEN $r0\n";
									saveAcc := false;
									instruction lexbuf
								}
	| "MINUS"	{ print "  MINUS $r0 $r0\n";
							saveAcc := true;
							instruction lexbuf
						}
	| "NOT"	{ print "  NEG $r0 $r0\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "ADD"	{ pop "$r1";
						print "  ADD $r0 $r1 $r0\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "SUB"	{ pop "$r1";
						print "  SUB $r0 $r1 $r0\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "MULT"	{ pop "$r1";
							print "  MULT $r0 $r1 $r0\n";
							saveAcc := true;
							instruction lexbuf
						}
	| "DIV"	{ pop "$r1";
						print "  DIV $r0 $r1 $r0\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "REM"	{ pop "$r1";
						print "  REM $r0 $r1 $r0\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "EQ"	{ pop "$r1";
						print "  EQ $r0 $r1 $r0\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "NEQ"	{ pop "$r1";
						print "  NEQ $r0 $r1 $r0\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "LT"	{ pop "$r1";
						print "  LT $r0 $r1 $r0\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "LE"	{ pop "$r1";
						print "  LE $r0 $r1 $r0\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "GT"	{ pop "$r1";
						print "  GT $r0 $r1 $r0\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "GE"	{ pop "$r1";
						print "  GE $r0 $r1 $r0\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "AND"	{ pop "$r1";
						print "  AND $r0 $r1 $r0\n";
						saveAcc := true;
						instruction lexbuf
					}
	| "OR"	{ pop "$r1";
						print "  OR $r0 $r1 $r0\n";
						saveAcc := true;
						instruction lexbuf
					}
	| digit+	{	if !saveAcc then push "$r0"; (* On sauvegarde ce qu'il y avait dans l'acc *)
              print ("  CONST $r0 " ^ (lexeme lexbuf) ^ "\n");
							saveAcc := true;
							instruction lexbuf
						}
	| id	{ if !saveAcc then push "$r0"; (* On sauvegarde ce qu'il y avait dans l'acc *)
          print ("  ADDRESS $r0 " ^ (lexeme lexbuf) ^ "\n");
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