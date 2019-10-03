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
}

let spaces = ['\n' ' ']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id = (letter | digit | '_')+
let commentLine = '#' [^ '\n']* '\n'

(* Les règles dans la partie .text *)
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
	| "PRINT"	{ pop "$r0";
 							print "  PRINT $r0\n";
							instruction lexbuf
						}
	| "READ"	{ pop "$r0";
							print "  READ $r0 $r0\n";
							push "$r0";
							instruction lexbuf
						}
	| "WRITE"	{ pop "$r0";
							pop "$r1";
							print "  WRITE $r1 $r0\n";
							instruction lexbuf
						}
	| "JUMP"	{ pop "$r0";
							print "  JUMP $r0\n";
							instruction lexbuf
						}
	| "JUMPWHEN"	{ pop "$r0";
									pop "$r1";
									print "  JUMP $r1 WHEN $r0\n";
									instruction lexbuf
								}
	| "MINUS"	{ pop "$r0";
							print "  MINUS $r0 $r0\n";
							push "$r0";
							instruction lexbuf
						}
	| "NOT"	{ pop "$r0";
						print "  NEG $r0 $r0\n";
						push "$r0";
						instruction lexbuf
					}
	| "ADD"	{ pop "$r0";
						pop "$r1";
						print "  ADD $r0 $r1 $r0\n";
						push "$r0";
						instruction lexbuf
					}
	| "SUB"	{ pop "$r0";
						pop "$r1";
						print "  SUB $r0 $r1 $r0\n";
						push "$r0";
						instruction lexbuf
					}
	| "MULT"	{ pop "$r0";
							pop "$r1";
							print "  MULT $r0 $r1 $r0\n";
							push "$r0";
							instruction lexbuf
						}
	| "DIV"	{ pop "$r0";
						pop "$r1";
						print "  DIV $r0 $r1 $r0\n";
						push "$r0";
						instruction lexbuf
					}
	| "REM"	{ pop "$r0";
						pop "$r1";
						print "  REM $r0 $r1 $r0\n";
						push "$r0";
						instruction lexbuf
					}
	| "EQ"	{ pop "$r0";
						pop "$r1";
						print "  EQ $r0 $r1 $r0\n";
						push "$r0";
						instruction lexbuf
					}
	| "NEQ"	{ pop "$r0";
						pop "$r1";
						print "  NEQ $r0 $r1 $r0\n";
						push "$r0";
						instruction lexbuf
					}
	| "LT"	{ pop "$r0";
						pop "$r1";
						print "  LT $r0 $r1 $r0\n";
						push "$r0";
						instruction lexbuf
					}
	| "LE"	{ pop "$r0";
						pop "$r1";
						print "  LE $r0 $r1 $r0\n";
						push "$r0";
						instruction lexbuf
					}
	| "GT"	{ pop "$r0";
						pop "$r1";
						print "  GT $r0 $r1 $r0\n";
						push "$r0";
						instruction lexbuf
					}
	| "GE"	{ pop "$r0";
						pop "$r1";
						print "  GE $r0 $r1 $r0\n";
						push "$r0";
						instruction lexbuf
					}
	| "AND"	{ pop "$r0";
						pop "$r1";
						print "  AND $r0 $r1 $r0\n";
						push "$r0";
						instruction lexbuf
					}
	| "OR"	{ pop "$r0";
						pop "$r1";
						print "  OR $r0 $r1 $r0\n";
						push "$r0";
						instruction lexbuf
					}
	| digit+	{	print ("  CONST $r0 " ^ (lexeme lexbuf) ^ "\n");
							push "$r0";
							instruction lexbuf
						}
	| id	{ print ("  ADDRESS $r0 " ^ (lexeme lexbuf) ^ "\n");
					push "$r0";
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

(* Les règles dans la partie .data *)
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