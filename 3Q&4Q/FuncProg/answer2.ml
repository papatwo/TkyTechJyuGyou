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
  Eq (Syntax.Var "v", Syntax.Var "x"));; (*store syntax tree in var*)

type 'a syn_tree =
  Empty
| Node of 'a * 'a syn_tree * 'a syn_tree;;



let rec exp tree = 
	match tree with
	|Syntax.Unit -> "()"
	|Syntax.Int(t1) -> string_of_int t1
	|Syntax.Bool(t1) -> string_of_bool t1 
	|Syntax.Let（(t11, t12), t2, t3）-> "let "^t11^" ="^exp t2^" in "^exp t3
	|Syntax.LetRec (t1, t2) -> "let rec "^t1.name
	|Eq (t1, t2) -> t1 = t2



	exp left @ [y] @ exp right ;;


let unp tree =
    let rec untok countS words =
      match words with
      |[] -> count
      |hd :: tl -> if (compare hd word == 0) then scan (count + 1) tl
      else scan count tl
    in scan 0 words;;

    let wc words word =
    let rec scan count words =
      match words with
      |[] -> count
      |hd :: tl -> if (compare hd word == 0) then scan (count + 1) tl
      else scan count tl
    in scan 0 words;;

let unp *)

#load "type.cmo"
#load "id.cmo"
#load "m.cmo"
#load "s.cmo"
#load "parser.cmo"
#load "lexer.cmo"
#load "typing.cmo"

let parse s =
  Parser.exp Lexer.token (Lexing.from_string s);;

(* let isSimple t =  match t with
| *)

(* The following is a totally wrong unparser implementation. Please ignore the definition and build an unparser from scratch. *)

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
  | Syntax.If (t1, t2, t3) -> "if " ^ (unparse t1) ^ " then \n" ^ (unparse t2) ^ " else \n" ^ (unparse t3)
  | Syntax.Let ((t1, t2), t3, t4) -> if (t2 = Unit) then
      "(" ^ (unparse t3) ^ ";\n" ^ (unparse t4) ^")" else
      "let " ^ (String.lowercase_ascii t1) ^ " = " ^ (unparse t3) ^ " in \n" ^ (unparse t4)
  | Syntax.Var v -> v
  | Syntax.LetRec (fd, t1) -> 
      "let rec " ^ (let elem e = match e with (idName, idType) -> idName in elem fd.name) ^ " " ^ (String.concat " " (List.map (function element -> match element with (idName, idType) -> idName) fd.args)) ^ " = \n" ^ (unparse fd.body) ^ " in \n" ^ (unparse t1)
  | Syntax.App(f, args) ->
      (unparse f) ^ " " ^ String.concat " " (List.map (function arg -> "(" ^ unparse arg ^ ")" ) args)
  | Syntax.Tuple (tl) -> "(" ^ String.concat ", " (List.map (function arg -> unparse arg) tl) ^ ")"
  | Syntax.LetTuple (list, t3, t4) -> "let " ^ "(" ^ String.concat "," (List.map (function element -> match element with (idName, idType) -> idName) list) ^ ")" ^ " = " ^ (unparse t3) ^ " in " ^ (unparse t4)
  | Syntax.Array (t1, t2) -> "Array.make " ^ (unparse t1) ^ " " ^ (unparse t2)
  | Syntax.Get (t1, t2) -> (unparse t1) ^ " .(" ^ (unparse t2) ^ ")"
  | Syntax.Put (t1, t2, t3) -> (unparse t1) ^ " .(" ^ (unparse t2) ^ ")" ^ " <- " ^ (unparse t3)
  | _ -> "fix_me"

let small_test_programs = [
  "()"; "print(true)"; "print(5)"; "print(3.14)";
  "print(not (4 = 5))"; "print(1 + 2 - 3)";
  "print(-1.0 +. 2.0 -. 3.0 *. 4.0 /. 5.0)";
  "print(3 < 4)"; "print(5 < 6)";
  "print(let x = 1 in let y = x in x + y)";
  "print(let x = let y = 1 in let z = 2 in y + z in x)";
  "print(let x = let rec f x = x + 1 in f 1 in x)";
  "print(let x = (1, 2) in let (a, b) = x in a + b)";
  "print(let x = 1 in let y = 2 in if y - 1 = x then x + y else y)";
  "print(let rec f n = if n <= 1 then 1 else f(n - 2) + f(n - 1) in f 5)";
  "print(let rec f n = if n = 0 then 1 else n + f (n - 1) in f(10))";
  "print(let a = 1 in let rec incr x = x + a in incr 5)";
  "let rec f x = let x1 = x + 1 in let x2 = x + 2 in let x3 = x + 3 in let x4 = x + 4 in let x5 = x + 5 in x1 + x2 + x3 + x4 + x5 in print(f 1)";
  "print(let (a, b) = (1, 2) in a + b)";
  "print(let a = Array.make 5 1.0 in a.(0) <- a.(1))" ];; 

let test_count = ref 1

let test label s =
  if label = "" then Printf.printf "Test case (%d): %s\n" (!test_count) s
  else Printf.printf "Test case (%d): [%s]\n" (!test_count) label;
  incr test_count;
  flush stdout;


  ignore (Typing.f (parse s));  (* Assert that the program is well typed *)

  Id.counter := 0; (* added *)
  let ast = parse s in

  let s = try unparse ast with e ->
    print_endline "Failure during unparsing";
    raise e in

  Id.counter := 0; (* added *)
  let pus = try parse s with e ->
    print_endline "Failure during parsing the output of your unparser";
    print_endline s;
    raise e in

  if pus = ast then print_endline "Beautiful!"
  else begin
    print_endline "Unsuccessful: Your unparser gave:";
    print_endline s;
  end;
  print_newline();;

(* Small tests *)
List.iter (test "") small_test_programs;;

(* Large tests *)
List.iter (fun path ->
  let fin = open_in path in
  let lines = ref [] in
  let program =
    try while true do lines := (input_line fin) :: (!lines) done; ""
    with End_of_file -> String.concat "\n" (List.rev (!lines)) in
  test path program)
  [ "shootout/ack.ml";
    "shootout/fib.ml";
    "shootout/harmonic.ml";
    "shootout/mandelbrot.ml";
    "shootout/tak.ml" ];;

(* Todo
 * - type check the tests
 * - exception handling
 *)