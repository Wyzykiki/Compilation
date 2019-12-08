open VAR
open FUNInstr
open IMPExpr
open Op

(**
   Version avec modèle mémoire limité, dans lequel on sépare la mémoire en
   plusieurs fragments :
   - le code,
   - la zone des variables globales,
   - pour chaque appel de fonction en cours, la zone de ses paramètres et la
     zone de ses variables locales,
   - la zone de mémoire principale (le tas, utilisé dans le module 3).
   Les variables globales, les paramètres et les variables
   locales ne peuvent être accédées que via leur nom. On ne leur associe pas
   d'adresse en mémoire. Seule la mémoire principale et les fonctions sont
   dotée d'adresses et peuvent être la cible d'un pointeur.

   Contrainte de forme sur les programmes pour que ce modèle convienne :
   - on ne peut procéder à un appel de fonction que via son identifiant
   - un identifiant [Name id] ne peut apparaître que dans l'une de ces
     situations :
     - sous l'opérateur [Deref]
     - à gauche d'une opération [Write]
     - comme fonction dans une opération [Call]
     - en tant que pointeur vers une fonction

   La valeur obtenue en évaluant une expression est un nombre entier,
   éventuellement interprétable comme une adresse dans la mémoire
   principale ou la mémoire des fonctions.
*)

type value = int

(**
   Exception à utiliser pour arrêter l'exécution d'une fonction et renvoyer
   son résultat.
*)
exception FunctionResult of value
      
(**
   Fonction principale, interprétant un programme complet
*)
let eval_program prog =
  (**
     Environnement global : table des fonctions 
     La table [functions] associe chaque nom de fonction à sa définition
     complète 
  *)
  let function_addresses : (string, int) Hashtbl.t = Hashtbl.create 17 in
  List.iteri
    (fun i fdef -> Hashtbl.add function_addresses fdef.name i)
    prog.text;
  let functions = Array.of_list prog.text in
  (**
     Enrivonnement global : variables globales
     La table [globals] simule la partie de la mémoire où sont stockées les
     valeurs des variables globales du programme, qu'on suppose n'accéder
     que via leur étiquette.
  *)
  let globals : (string, value) Hashtbl.t = Hashtbl.create 17 in
  List.iter (fun (id, v) -> Hashtbl.add globals id v) prog.globals;
  (**
     Environnement global : mémoire principale
     Le tableau [memory] simule la mémoire principale, qui contiendra le tas
     dans le module 3. Les pointeurs désignent des adresses dans ce tableau.
  *)
  let memory = Array.make 65536 0 in

  (**
     Fonction d'interprétation des fonctions, les valeurs de ses paramètres
     étant données sous la forme d'une liste.
  *)
  let rec eval_function fdef param_values =
    (** Table des paramètres, similaire à [globals] *)
    let params : (string, value) Hashtbl.t = Hashtbl.create 17 in
    List.iter2
      (fun id v -> Hashtbl.add params id v)
      fdef.parameters param_values;
    (** Table des variables locales, similaire à [globals] *)
    let locals : (string, value) Hashtbl.t = Hashtbl.create 17 in
    List.iter
      (fun (id, v) -> Hashtbl.add locals id v)
      fdef.locals;
    (** 
        Évaluation du corps de la fonction.
        On s'attend à ce que l'une de ces deux conditions se réalise :
        - la fonction renvoie un résultat, avec une exception [FunctionResult]
        - la fonction interrompt le programme avec [exit]
    *)
    try
      eval_sequence fdef.code params locals;
      failwith "missing return or exit statement"
    with
      | FunctionResult(v) -> v

  (** Fonction d'interprétation d'une séquence d'instructions *)
  and eval_sequence s params locals =
    List.iter (fun i -> eval_instruction i params locals) s

  (** Fonction d'interprétation d'une instruction *)
  and eval_instruction i params locals =

    (** 
        Préambule : fonction d'évaluation d'une expression, dans le
        contexte [globals/params/locals] correspondant à l'appel de
        fonction englobant.
    *)
    let rec eval_expr = function
      | Immediate(n) -> n
      | Unop(op, e) -> interpret_unop op (eval_expr e)
      | Binop(op, e1, e2) -> interpret_binop op (eval_expr e1) (eval_expr e2)
      (** Les noms de variables isolés ne peuvent que désigner des
          fonctions. *)
      | Name(f) -> Hashtbl.find function_addresses f 
      (** Le déréférencement d'un nom consulte les différentes tables.
          Le masquage des variables globales par les paramètres de fonction
          et les variables locales est réalisé implicitement par l'ordre dans
          lequel les tables sont consultées. *)
      | Deref(Name(id)) ->
        begin match Hashtbl.find_opt locals id with
          | Some(v) -> v
          | None -> match Hashtbl.find_opt params id with
              | Some(v) -> v
              | None -> Hashtbl.find globals id 
        end
      (** Le déréférencement d'un pointeur consulte la mémoire. *)
      | Deref(e) -> memory.(eval_expr e)
    in

    (** Interprétation de l'instruction *)
    match i with
      | Nop -> ()
      | Print(e) ->
        let v = eval_expr e in
        Printf.printf "%c" (char_of_int v)
      | Exit -> exit 0
      | If(e, s1, s2) ->
        if eval_expr e <> 0
        then eval_sequence s1 params locals
        else eval_sequence s2 params locals
      | While(e, s) ->
        while eval_expr e <> 0
        do eval_sequence s params locals done
        
      (** Utilisation de [Write] avec pour cible un identifiant : affectation 
          d'une nouvelle valeur pour une variable. Comme pour le déréférencement
          on consulte les différentes tables par ordre de priorité. Ici en plus,
          on interdit de modifier la valeur d'un paramètre de fonction. *)
      | Write(Name id, e) ->
        let v = eval_expr e in
        begin match Hashtbl.find_opt locals id with
          | Some(_) -> Hashtbl.replace locals id v
          | None -> match Hashtbl.find_opt params id with
              | Some(_) -> failwith "function parameters are read-only"
              | None -> Hashtbl.replace globals id v
        end
      (** Utilisation de [Write] avec pour cible un pointeur : écriture dans
          la mémoire principale. *)
      | Write(e1, e2) ->
        memory.(eval_expr e1) <- eval_expr e2

      (** Appel de fonction, avec distinction de la nature de l'emplacement
          cible (variable ou pointeur). On demande à ce que la fonction soit
          désignée par son identifiant. *)
      | Call(d, f, args) ->
        let values = (List.map eval_expr args) in
        let fa = eval_expr f in
        let v = eval_function functions.(fa) values in
        begin match d with
          | Name id ->
            begin match Hashtbl.find_opt locals id with
              | Some(_) -> Hashtbl.replace locals id v
              | None -> match Hashtbl.find_opt params id with
                  | Some(_) -> failwith "function parameters are read-only"
                  | None -> Hashtbl.replace globals id v
            end
          | _ -> memory.(eval_expr d) <- v
        end
      (* | Call(_, _, _) -> *)
      (*   failwith "functions must be referenced by name" *)

      (** Renvoi du résultat d'une fonction : exception [FunctionResult] *)
      | Return(e) -> raise (FunctionResult(eval_expr e))
  in

  (** Interpréter un programme revient à interpréter sa fonction [main] *)
  let main = functions.(Hashtbl.find function_addresses "main") in
  eval_function main []
