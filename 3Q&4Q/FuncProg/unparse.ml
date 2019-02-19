(* (*write a function in string and use ast to get its syntax tree*)
let p = "let x = 3 in v x = x+x+x";;
ast p;;

(*assign lexing buffer of p into a var b*)
let lex = Lexing.from_string;; (*rename lexing with a shorter name*)
let b = lex p;; (*store all the lexing buffer of p*)
let tok = Lexer.token;; (*rename tokenization with a shorter name*)
let t = tok b;; (*loop this to tokenize lexing buffer*)
let t = ast p;; (*store syntax tree in var*)


let t =
  Syntax.Let (("x", Var {contents = None}), Syntax.Int 3,
  Eq (Syntax.Var "v", Syntax.Var "x"));; (*store syntax tree in var*) *)




let rec unparse tree = 
	match tree with
	|Syntax.Unit -> "()"
	|Syntax.Bool (t1) -> string_of_bool t1 
	|Syntax.Int (t1) -> string_of_int t1
	|Syntax.Float (t1) -> string_of_float t1
	|Syntax.Not (t1) -> "(not " ^ unparse t1 ^ ")"
	|Syntax.Neg (t1) -> "- " ^ (unparse t1)
	|Syntax.Add (t1, t2) -> (unparse t1) ^ " + " ^ (unparse t2) 
	|Syntax.Sub (t1, t2) -> (unparse t1) ^ " - " ^ (unparse t2)
	|Syntax.FNeg (t1) -> "-" ^ (unparse t1)
	|Syntax.FAdd (t1, t2) -> (unparse t1) ^ " +. " ^ (unparse t2)
	|Syntax.FSub (t1, t2) -> (unparse t1) ^ " -. " ^ (unparse t2)
	|Syntax.FMul (t1, t2) -> (unparse t1) ^ " *. " ^ (unparse t2)
	|Syntax.FDiv (t1, t2) -> (unparse t1) ^ " /. " ^ (unparse t2)
	|Syntax.Eq (t1, t2) -> (unparse t1) ^ " = " ^ (unparse t2)
	|Syntax.LE (t1, t2) -> (unparse t1) ^ " < " ^ (unparse t2)
	|Syntax.If (t1, t2, t3) -> "if " ^ (unparse t1) ^ " then " ^ (unparse t2) ^ " else " ^ (unparse t3)
	|Syntax.Let ((t1, t2), t3, t4) -> "let " ^ (t1) ^ " = " ^ (unparse t3) ^ " in " ^ (unparse t4)
	|Syntax.Var (t1) -> t1
	|Syntax.LetRec (fd, t1) -> "let rec " ^ (let elem e = match e with (idName, idType) -> idName in elem fd.name) ^ " " ^ (String.concat "," (List.map (function element -> match element with (idName, idType) -> idName) fd.args)) ^ " = " ^ (unparse fd.body) ^ " in " ^ (unparse t1)
    |Syntax.App (t1, tl) -> (unparse t1) ^ " " ^ String.concat " " (List.map (function arg -> unparse arg) tl)
    |Syntax.Tuple (tl) -> "(" ^ String.concat ", " (List.map (function arg -> unparse arg) tl) ^ ")"
    |Syntax.LetTuple (list, t3, t4) -> "let " ^ "(" ^ String.concat "," (List.map (function element -> match element with (idName, idType) -> idName) list) ^ ")" ^ " = " ^ (unparse t3) ^ " in " ^ (unparse t4)
	|Syntax.Array (t1, t2) -> (unparse t1) ^ (unparse t2)
	|Syntax.Get (t1, t2) -> (unparse t1) ^ " .(" ^ (unparse t2) ^ ")"
	|Syntax.Put (t1, t2, t3) -> (unparse t1) ^ " .(" ^ (unparse t2) ^ ")" ^ " <- " ^ (unparse t3)






let rec unparse (ast : Syntax.t) =
  match ast with
  | Syntax.Unit -> "()"
  | Syntax.Bool t1 -> string_of_bool t1 
  | Syntax.Int n -> string_of_int n
  | Syntax.Float t1 -> string_of_float t1
  | Syntax.Not e -> "(not " ^ unparse e ^ ")"
  | Syntax.Neg t1 -> "- " ^ (unparse t1)
  | Syntax.Add (t1, t2) -> (unparse t1) ^ " + " ^ (unparse t2) 
  | Syntax.Sub (t1, t2) -> "(" ^ (unparse t1) ^ " - " ^ (unparse t2) ^ ")"
  | Syntax.FNeg t1 -> "-" ^ (unparse t1)
  | Syntax.FAdd (t1, t2) -> "(" ^ (unparse t1) ^ " +. " ^ (unparse t2) ^ ")"
  | Syntax.FSub (t1, t2) -> "(" ^ (unparse t1) ^ " -. " ^ (unparse t2) ^ ")"
  | Syntax.FMul (t1, t2) -> "(" ^ (unparse t1) ^ " *. " ^ (unparse t2) ^ ")"
  | Syntax.FDiv (t1, t2) -> "(" ^ (unparse t1) ^ " /. " ^ (unparse t2) ^ ")"
  | Syntax.Eq (e1, e2) ->  "(" ^ unparse e1 ^ " = " ^ unparse e2 ^ ")"
  | Syntax.LE (t1, t2) -> "(" ^ (unparse t1) ^ " <= " ^ (unparse t2) ^ ")"
  | Syntax.If (t1, t2, t3) -> "(" ^ "if " ^ (unparse t1) ^ " then " ^ (unparse t2) ^ " else " ^ (unparse t3) ^ ")"
  | Syntax.Let ((t1, t2), t3, t4) ->  
      "let " ^ (t1) ^ " = " ^ (unparse t3) ^ " in " ^ (unparse t4)
  | Syntax.Var v -> v
  | Syntax.LetRec (fd, t1) -> 
      "let rec " ^ (let elem e = match e with (idName, idType) -> idName in elem fd.name) ^ " " ^ (String.concat "," (List.map (function element -> match element with (idName, idType) -> idName) fd.args)) ^ " = " ^ (unparse fd.body) ^ " in " ^ (unparse t1)
  | Syntax.App(f, args) ->
      (unparse f) ^ "(" ^ String.concat " " (List.map (function arg -> unparse arg) args) ^ ")"
  | Syntax.Tuple (tl) -> "(" ^ String.concat ", " (List.map (function arg -> unparse arg) tl) ^ ")"
  | Syntax.LetTuple (list, t3, t4) -> "let " ^ "(" ^ String.concat "," (List.map (function element -> match element with (idName, idType) -> idName) list) ^ ")" ^ " = " ^ (unparse t3) ^ " in " ^ (unparse t4)
  | Syntax.Array (t1, t2) -> (unparse t1) ^ (unparse t2)
  | Syntax.Get (t1, t2) -> (unparse t1) ^ " .(" ^ (unparse t2) ^ ")"
  | Syntax.Put (t1, t2, t3) -> (unparse t1) ^ " .(" ^ (unparse t2) ^ ")" ^ " <- " ^ (unparse t3)
  | _ -> "fix_me"