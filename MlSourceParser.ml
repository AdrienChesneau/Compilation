(* Type des lexèmes *)

type token =
  | MAIN
  | BEGIN | END  (* {, } *)
  | IDENT of string
  | INT of int
  | BOOL of bool
  | SEMI  (* ; *)
  | PRINT | WHILE
  | IF | ELSE
  | SET  (* := *)
  | LP | RP (* (, ) *)
  | PLUS | MINUS | STAR | DIV | MOD
  | EQUAL | NEQ | LE | LT | GE | GT (* ==, !=, <, <=, >, >= *)
  | AND | OR | NOT
  | VAR
  | INTEGER | BOOLEAN
  | EOF | BOF  (* fin de fichier, début de fichier *)

(**
   Structure de données représentant l'entrée en cours de lecture,
   et fonctions associées.

   Les composantes essentielles sont la chaîne de caractères en cours d'analyse
   et un curseur indiquant la position courante.
*)      
type input_buffer = {
  (* Mot d'entrée et sa longueur *)
  input: string;
  length: int;
  (* Début du mot analysé, et position courante *)
  mutable start_pos: int;
  mutable next_pos: int;
  (* Pour l'enregistrement des positions *)
  mutable current_line: int;
  mutable current_col: int;
}

(* Initialisation *)
let init_buffer s = {
  input = s;
  length = String.length s;
  start_pos = 0;
  next_pos = 0;
  current_line = 1;
  current_col = 1;
}

exception Eof
  
(* Lire le caractère courant *)
let next_char b =
  if   b.next_pos < b.length
  then b.input.[b.next_pos]
  else raise Eof

(* Faire avancer le curseur *)
let shift b =
  b.next_pos <- b.next_pos + 1;
  b.current_col <- b.current_col + 1

(* Enregistrer un retour à la ligne *)
let new_line b =
  b.current_line <- b.current_line + 1;
  b.current_col <- 1
    
(* Marquer le début du lexème en cours d'analyse *)
let init_pos b = b.start_pos <- b.next_pos

(* Obtenir le lexème analysé *)
let current_word b =
  String.sub b.input b.start_pos (b.next_pos - b.start_pos)

(**
   Représentation de l'automate.

   La fonction [read_token: input_buffer -> token] correspond à l'état initial.
   Elle renvoie le prochain lexème et fait progresser le curseur en conséquence
   dans le texte source.
*)
    
let rec read_token b =
  try  match next_char b with
    (* Un seul caractère : on avance, et on renvoie le lexème correspondant. *)
    | '{' -> shift b; BEGIN
    | '}' -> shift b; END
    | '(' -> shift b; LP
    | ')' -> shift b; RP
    | '+' -> shift b; PLUS
    | '-' -> shift b; MINUS
    | '*' -> shift b; STAR
    | '/' -> shift b; DIV
    | '%' -> shift b; MOD      
    | '<' -> shift b; read_lt b
    | '>' -> shift b; read_gt b
    | '&' -> shift b; read_and b
    | '|' -> shift b; read_or b
    | '!' -> shift b; NOT
    | ';' -> shift b; SEMI
    (* Lexème potentiellement formé de plusieurs caractères : transition vers
       un nouvel état, c'est-à-dire appel d'une autre fonction.
       Si besoin, on initialise le curseur de début de lexème à ce moment. *)
    | ':' -> shift b; read_set b
    | c when 'a' <= c && c <= 'z' -> init_pos b; shift b; read_word b
    | c when '0' <= c && c <= '9' -> init_pos b; shift b; read_int b
    (* On ignore les blancs (espaces, tabulations, retours chariot, etc.).
       Dans ce cas, on relance immédiatement l'analyse à partir du caractère
       suivant avec un nouvel appel à [read_token]. *)
    | ' '  -> shift b; read_token b
    | '\n' -> shift b; new_line b; read_token b
    (* Tout autre caractère est une erreur. *)
    | c   -> failwith (Printf.sprintf "Unrecognized character: %c" c)
  with
    | Eof -> EOF

(**
   Fonctions auxiliaires correspondant aux autres états de l'automate.
*)

and read_set b =
  match next_char b with
    (* On attend := *)
    | '=' -> shift b; SET
    (* Échec sinon *)
    | c   -> failwith (Printf.sprintf "Unrecognized character: %c" c)

and read_lt b =
  match next_char b with
    (* On peut lire <= *)
    | '=' -> shift b; LE
    (* ou rester sur < *)
    | c   -> LT

and read_gt b =
  match next_char b with
    (* On peut lire >= *)
    | '=' -> shift b; GE
    (* Ou rester sur > *)
    | c   -> GT

and read_and b =
  match next_char b with
    (* On attend && *)
    | '&' -> shift b; AND
    (* Échec sinon *)
    | c   -> failwith (Printf.sprintf "Unrecognized character: %c" c)

and read_or b =
  match next_char b with
    (* On attend || *)
    | '|' -> shift b; OR
    (* Échec sinon *)
    | c   -> failwith (Printf.sprintf "Unrecognized character: %c" c)

      
(* Reconnaissance d'un entier *)
and read_int b =
  match next_char b with
    (* Tant qu'on lit des chiffres, on avance et on continue avec le même état,
       c'est-à-dire la même fonction. *)
    | c when '0' <= c && c <= '9' -> shift b; read_int b
    (* Sinon on renvoie l'entier reconnu, et on préserve le prochain
       caractère. *)
    | _ -> INT (int_of_string (current_word b))
      
(* Lecture d'un mot *)
and read_word b =
  match next_char b with
    | c when 'a' <= c && c <= 'z' -> shift b; read_word b
    | _ -> (match current_word b with
	(* On commence par vérifier si le mot est un mot-clé. *)
	| "main" -> MAIN
	| "print" -> PRINT
	| "while" -> WHILE
        | "if" -> IF
        | "else" -> ELSE
        | "true" -> BOOL true
        | "false" -> BOOL false
        | "var" -> VAR
        | "integer" -> INTEGER
        | "boolean" -> BOOLEAN
	(* Sinon, c'est un identificateur. *)
	| id -> IDENT id
    )
(* Pour retrouver le mot courant, on aurait aussi pu ajouter un accumulateur
   à la fonction [read_word]. *)
      

let token_to_string = function
  | AND -> "AND"
  | BEGIN -> "BEGIN"
  | BOF -> "START"
  | BOOL b -> if b then "BOOL true" else "BOOL false"
  | BOOLEAN -> "BOOLEAN"
  | DIV -> "DIV"
  | ELSE -> "ELSE"
  | END -> "END"
  | EOF -> "EOF"
  | EQUAL -> "EQUAL"
  | GE -> "GE"
  | GT -> "GT"
  | IDENT s -> Printf.sprintf "IDENT %s" s
  | IF -> "IF"
  | INT i -> Printf.sprintf "INT %d" i
  | INTEGER -> "INTEGER"
  | LE -> "LE"
  | LP -> "LP"
  | LT -> "LT"
  | MAIN -> "MAIN"
  | MINUS -> "MINUS"
  | MOD -> "MOD"
  | NEQ -> "NEQ"
  | NOT -> "NOT"
  | OR -> "OR"
  | PLUS -> "PLUS"
  | PRINT -> "PRINT"
  | RP -> "RP"
  | SEMI -> "SEMI"
  | SET -> "SET"
  | STAR -> "STAR"
  | VAR -> "VAR"
  | WHILE -> "WHILE"

      
(**
   Grammaire
   ---------

  [program] <-  BOF [globals] main [block] EOF

  [globals] <-  <epsilon>
             |  var [typ] <ident> ; [globals]

      [typ] <-  integer
             |  boolean

    [block] <-  { } 
             |  { [instr] }

    [instr] <-  <epsilon>
             |  print ( [expr] )
             |  <ident> := [expr]
             |  while ( [expr] ) [block]
             |  if ( [expr] ) [block] else [block]
             |  [instr] ; [instr]

     [expr] <-  <int>
             |  <ident>
             |  ( [expr] )
             |  [uop] [expr]
             |  [expr] [bop] [expr]

      [uop] <-  -  |  !
      [bop] <-  +  |  -  |  *  |  /  |  %
             |  ==  |  !=  |  <  |  <=  |  >  |  >=
             |  &&  |  ||

   On essaye de regrouper les différents lexèmes en suivant les règles de
   grammaire du langage. On produit un Arbre de Syntaxe abstraiTe (AST) qui
   représente le programme de manière structurée.
*)

(**
   Syntaxe abstraite
   -----------------
   On récupère celle définir par ailleurs.
*)

open CommonAST
open SourceLocalisedAST

(**
   Analyse syntaxique
   ------------------
*)

(**
   Structure de données représentant la séquence de lexèmes en cours d'analyse,
   et fonctions associées.
 *)
type token_buffer = {
  (* Entrée brute, d'où lire les lexèmes *)
  input: input_buffer;
  (* Lexème courant, initialisé à [BOF] *)
  mutable next_token: token;
}
let get_pos b = b.input.current_line, b.input.current_col

(* Consulter le lexème courant *)
let next_token b = b.next_token

(* Faire avancer le curseur : lire le prochain lexème de l'entrée *)
let shift b = b.next_token <- read_token b.input

(* Initialisation *)
let init_token_buffer s =
  { input = init_buffer s; next_token = BOF }
  
(**
   Fonction principale : construit l'arbre syntaxique
*)
(* [parse_program: token_buffer -> program] *)
(* [main] <-  BOF [var_decls] main [block] EOF *)
let rec parse_program b =
  (* On a prévu que les premiers lexèmes soient toujours BOF et main *)
  expect_token BOF b;
  let globals = parse_globals b in
  expect_token MAIN b;
  (* On rencontre ensuite le non terminal [block] : appel récursif dont
     on récupère le résultat. *)
  let main = parse_block b in
  (* Enfin, on doit finir par EOF *)
  expect_token EOF b;
  (* Une fois la règle reconnue, on renvoie la construction correspondante,
     de type [program].
  *)
  { main; globals }

(* Vérification de l'identité du prochain terminal. *)
and expect_token t b =
  if t = next_token b
  then shift b
  else failwith (Printf.sprintf "Syntax error : %s expected" (token_to_string t))

(* [parse_globals: token_buffer -> typ Symb_Tbl.t] *)
and parse_globals b =
  if next_token b = VAR
  then begin
    shift b;
    let ty = parse_type b in
    let id = expect_ident b in
    expect_token SEMI b;
    let vds = parse_globals b in
    Symb_Tbl.add id ty vds
  end
  else Symb_Tbl.singleton "arg" TypInt

and parse_type b =
  match next_token b with
    | INTEGER -> shift b; TypInt
    | BOOLEAN -> shift b; TypBool
    | _ -> failwith "Syntax error : type expected"
    
(* [parse_block: token_buffer -> instruction] *)
(* [block] <- BEGIN END 
            | BEGIN [instr] END *)
and parse_block b =
  expect_token BEGIN b;
  if next_token b = END
  then begin shift b; mk_loc_i Nop b end
  else begin
    let i = parse_instr b in
    expect_token END b;
    i
  end

(* La production
   [instr] <-  [instr] ; [instr]
   donnée pour les instructions n'étant pas compatible avec l'analyse récursive
   descendante, on transforme la grammaire

   [instr] <-  print ( [expr] )
            |  <ident> := [expr]
            |  while ( [expr] ) [block]
            |  if ( [expr] ) [block] else [block]
            |  [instr] ; [instr]

   des instructions en la suivante :

   [instr] <-  [s_instr]
            |  [s_instr] ; [instr]

 [s_instr] <-  print ( [expr] )
            |  <ident> := [expr]
            |  while ( [expr] ) [block]
            |  if ( [expr] ) [block] else [block]
*)
and parse_instr b =
  let i1 = parse_s_instr b in
  match next_token b with
    | SEMI -> shift b; let i2 = parse_instr b in mk_loc_i (Sequence(i1, i2)) b
    | _    -> i1
      
and parse_s_instr b =
  match next_token b with
    | PRINT -> shift b; expect_token LP b; let e = parse_expr b in
					   expect_token RP b;
					   mk_loc_i (Print(e)) b
    | IDENT id -> shift b; expect_token SET b; let e  = parse_expr b in
                                               mk_loc_i (Set(Identifier(Id id), e)) b
    | WHILE -> shift b; expect_token LP b; let e = parse_expr b in
                                           expect_token RP b;
                                           let i = parse_block b in
                                           mk_loc_i (Loop(e, i)) b
    | IF -> shift b; expect_token LP b; let e = parse_expr b in
                                        expect_token RP b;
                                        let i1 = parse_block b in
                                        expect_token ELSE b;
                                        let i2 = parse_block b in
                                        mk_loc_i (Conditional(e, i1, i2)) b
    | t -> failwith (Printf.sprintf "Bad instruction on token %s" (token_to_string t))

and mk_loc_i instr b =
  { instr; i_pos = get_pos b }
      
(* [expect_ident: token_buffer -> string] *)
and expect_ident b =
  match next_token b with
    | IDENT s -> shift b; s
    | t    -> failwith "Ident expected"

(* De même que pour les instructions, nous avons besoin de transformer la
   grammaire des expressions. On prend la grammaire LL classique pour les
   expressions arithmétiques.

     [expr] <- [l_expr]              // Expression

   [l_expr] <- [c_expr] [l_expr']    // Expression logique
  [l_expr'] <- <epsilon>
             | [lop] [l_expr]
      [lop] <- && | ||

   [c_expr] <- [a_expr] [c_expr']    // Expression de comparaison
  [c_expr'] <- <epsilon>
             | [cop] [c_expr]
      [cop] <- == | != | < | <= | > | >=

   [a_expr] <- [m_expr] [a_expr']    // Expression additive
  [a_expr'] <- <epsilon>
             | [aop] [a_expr]
      [aop] <- + | - 

   [m_expr] <- [s_expr] [m_expr']    // Expression multiplicative
  [m_expr'] <- <epsilon>
             | [mop] [m_expr]
      [mop] <- * | / | %

   [s_expr] <- <int>                 // Expression simple
             | <ident>
             | ( [expr] )
             | [uop] [s_expr]
      [uop] <- - | !
*)
and parse_expr b = parse_l_expr b
  
and parse_l_expr b =
  let ce = parse_c_expr b in
  match next_token b with
    | AND | OR as op ->
      shift b; let le = parse_l_expr b in
               let op = match op with
                 | AND -> And
                 | OR -> Or
                 | _ -> assert false
               in mk_loc_e (BinaryOp(op, ce, le)) b
    | _ -> ce

and parse_c_expr b =
  let ae = parse_a_expr b in
  match next_token b with
    | EQUAL | NEQ | LT | LE | GT | GE as op ->
      shift b; let ce = parse_c_expr b in
               let op = match op with
                 | EQUAL -> Eq
                 | NEQ -> Neq
                 | LT -> Lt
                 | LE -> Le
                 | GT -> Gt
                 | GE -> Ge
                 | _ -> assert false
               in mk_loc_e (BinaryOp(op, ae, ce)) b
    | _ -> ae

and parse_a_expr b =
  let me = parse_m_expr b in
  match next_token b with
    | PLUS | MINUS as op ->
      shift b; let ae = parse_a_expr b in
               let op = match op with
                 | PLUS -> Add
                 | MINUS -> Sub
                 | _ -> assert false
               in mk_loc_e (BinaryOp(op, me, ae)) b
    | _ -> me

and parse_m_expr b =
  let se = parse_s_expr b in
  match next_token b with
    | STAR | DIV | MOD as op ->
      shift b; let me = parse_m_expr b in
               let op = match op with
                 | STAR -> Mult
                 | DIV -> Div
                 | MOD -> Mod
                 | _ -> assert false
               in mk_loc_e (BinaryOp(op, se, me)) b
    | _ -> se
      
and parse_s_expr b =
  match next_token b with
    | INT i -> shift b; mk_loc_e (Literal (Int i)) b
    | BOOL v -> shift b; mk_loc_e (Literal (Bool v)) b
    | IDENT id -> shift b; mk_loc_e (Location (Identifier(Id id))) b
    | LP -> shift b; let e = parse_expr b in
                     let _ = expect_token RP b in
                     e
    | MINUS | NOT as op -> shift b; let e = parse_expr b in
                                    let op = match op with
                                      | MINUS -> Minus
                                      | NOT -> Not
                                      | _ -> assert false
                                    in mk_loc_e (UnaryOp(op, e)) b
    | t -> failwith (Printf.sprintf "Bad expression : %s not allowed" (token_to_string t))

and mk_loc_e expr b =
  { expr; e_pos = get_pos b }
      
let parse_text t =
  let b = init_token_buffer t in
  parse_program b
