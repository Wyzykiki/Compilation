module C = CLLInstr
module I = IMPInstr

let new_return_label =
  let cpt = ref 0 in
  fun () -> incr cpt; Printf.sprintf "_return_label_%i" !cpt

let rec translate_instruction = function
  | C.Nop -> [ I.Nop ]
  | C.Print(e) -> [ I.Print(e) ]
  | C.Exit -> [ I.Exit ]
  | C.Write(le, e) -> [ I.Write(le, e) ]
  | C.If(c, s1, s2) -> [ I.If(c, translate_sequence s1, translate_sequence s2) ]
  | C.While(c, s) -> [ I.While(c, translate_sequence s) ]
  | C.Call(e) -> 
    (* Création d'une nouvelle étiquette pour désigner la position où il faudra revenir juste après l'appel. *)
    let return = new_return_label() in
    [
      (* Protocole, étape 1 : enregistrement de l'adresse de retour. *)
      I.Write(IMPExpr.Name "return_address", IMPExpr.Name return);
      (* Appel à proprement parler : on passe la main à l'appelé. *)
      I.Goto(e);
      (* Point auquel revenir après l'appel. *)
      I.Label(return)
      (* Protocole, étape 5 : rien de particulier, seulement continuer. *)
    ]

  | C.Return -> 
  (* Protocole, étape 4 *)
  (* puis retour à l'appelant *)
  (* alternativement, saut à la séquence de fin de la fonction *)
    (* SP est restauré à FP *)
  [ I.Write(IMPExpr.Name "stack_pointer", IMPExpr.Deref(IMPExpr.Name "frame_pointer")) ]
  
    (* Les valeurs de FP et RA sont restauré pour l'appelant et SP est de retour en haut de la frame de l'appelant *)
  @ (I.pop (IMPExpr.Name "frame_pointer"))
  @ (I.pop (IMPExpr.Name "return_address"))

  (* Passer la main à nouveau à l'appelant. *)
  @ [ I.Goto(IMPExpr.Deref(IMPExpr.Name "return_address")) ]

and translate_sequence s =
  List.flatten (List.map translate_instruction s)

(* Le code qui sera produit pour la fonction fdef.
   C'est là qu'on retrouvera les parties du protocole à faire réaliser par l'appelé.
*)
let translate_function_definition fdef =
  (* Récupération du nom de la fonction. *)
  let f_name = CLL.(fdef.name) in
  (* Étiquette portant le nom de la fonction, où sauter à la fin de l'étape 1 du protocole. *)
  [ I.Label(f_name) ]

  (* Protocole, étape 2 : on fait une sauvegarde sur la pile des valeurs courantes de frame_pointer et return_address. *)
    
    (* Adresse (de l'étiquette) de retour dans l'appelant *)
  @ (I.push (IMPExpr.Deref (IMPExpr.Name "return_address")))
  
    (* Adresse du bas de la frame de l'appelant (à partir de laquelle elle enregistre ses variables locales) *)
  @ (I.push (IMPExpr.Deref (IMPExpr.Name "frame_pointer")))

    (* FP prend la valeur de SP *)
  @ [ I.Write(IMPExpr.Name "frame_pointer", IMPExpr.Deref(IMPExpr.Name "stack_pointer")) ]

  (* Protocole, étape 3 : exécuter le corps de la fonction. *)
  @ (translate_sequence CLL.(fdef.code))

  (* Protocole, étape 4 : défaire ce qui a été fait à l'étape 2 (notez la symétrie !). *)
  
    (* SP est restauré à FP *)
  @ [ I.Write(IMPExpr.Name "stack_pointer", IMPExpr.Deref(IMPExpr.Name "frame_pointer")) ]
  
    (* Les valeurs de FP et RA sont restauré pour l'appelant et SP est de retour en haut de la frame de l'appelant *)
  @ (I.pop (IMPExpr.Name "frame_pointer"))
  @ (I.pop (IMPExpr.Name "return_address"))

  (* Passer la main à nouveau à l'appelant. *)
  @ [ I.Goto(IMPExpr.Deref(IMPExpr.Name "return_address")) ]

    
let translate_program prog = {
  IMP.text =
    (* On s'assure de commencer par la fonction main *)
    [ I.Goto(IMPExpr.Name("main")) ]
    @ (List.flatten (List.map translate_function_definition CLL.(prog.text)));
  IMP.data =
    (* Les variables globales frame_pointer et return_address doivent être ajoutées à la liste déjà présente. *)
    CLL.(prog.data) @ [ ("frame_pointer", 65536) ] @ [ ("return_address", 65536) ] 
}
