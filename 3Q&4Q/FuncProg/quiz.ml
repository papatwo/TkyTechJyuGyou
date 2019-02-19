type 'a binary_tree =
  Empty
| Node of 'a * 'a binary_tree * 'a binary_tree;;


let tree = Node(4, Node(2, Node(1, Empty, Empty), Node(3, Empty, Empty)), Node(5, Empty, Empty));;
 
(*QUIZ VALUES*)
 (* inefficient solution *)
let rec values tree = 
	match tree with
	|Empty -> []
	|Node(y, left, right) -> values left @ [y] @ values right;;

let values tree =
	let rec traverse t values = 
	match t with
	| Empty -> values
	| Node(y, left, right) -> traverse right (x::traverse left values)
	in
	List.rev (traverse t []);;

(*
let values tree = 
	let rec printleft t values=
		match t with
			| Empty -> values
			| Node(y, left, right) -> y::printleft ;;
		*)

let rec v t l = 
	match t with
	Empty -> l
	| Node (va, le ,ri) -> let rl = v ri l in v le (va::rl);;

let values tree = v tree [];;

(*QUIZ MIRROR*)
let rec mirror t = 
	match t with
	| Empty -> Empty
	| Node(x, left, right) -> Node(x, mirror left, mirror right);;(*left right should be reverse*)

let rec mirror t =
  match t with
    | Empty -> Empty
    | Node(x, left, right) -> Node(x, mirror right, mirror left);;


(*QUIZ IS_BINARY_SEARCH_TREE*)
(*let is_bianry_search_tree t = 
	match t with
	|Empty -> False
	|Node(x, left, right) -> True;;

let is_bianry_search_tree t =
  match t with                 
  |Empty -> false              
  |Node(x,left,right) ->true;;*)

  let rec mirror t =
  match t with
    | Empty -> Empty
    | Node(x, left, right) -> Node(x, mirror right, mirror left);;

assert(values(a_binary_search_tree1) = List.rev(values(mirror(a_binary_search_tree1))));;
assert(values(not_a_binary_search_tree1) = List.rev(values(mirror(not_a_binary_search_tree1))));;

let is_binary_search_tree t =
  let rec aux test t =
    match t with
      | Empty -> true
      | Node(y, left, right) ->
          test y &&
          aux (function x -> test x && x < y) left &&
          aux (function x -> test x && x > y) right in
  aux (function _ -> true) t;;
    
assert(is_binary_search_tree a_binary_search_tree1);;
assert(not (is_binary_search_tree not_a_binary_search_tree1));;
assert(not (is_binary_search_tree not_a_binary_search_tree2));;

let rec is_binary_search_tree t =
  let rec aux x values =
    match values with
      | [] -> true
      | y :: values' -> x < y && aux y values' in
  aux min_int (values t);;

assert(is_binary_search_tree a_binary_search_tree1);;
assert(not (is_binary_search_tree not_a_binary_search_tree1));;
assert(not (is_binary_search_tree not_a_binary_search_tree2));;

let rec is_binary_search_tree t =
  let rec aux (l, r) t =
    match t with
    | Empty -> true
    | Node(v, left, right) ->
        l < v && v < r && aux (l, v) left && aux(v, r) right in
  aux (min_int, max_int) t;;

assert(is_binary_search_tree a_binary_search_tree1);;
assert(not (is_binary_search_tree not_a_binary_search_tree1));;
assert(not (is_binary_search_tree not_a_binary_search_tree2));;
