let pi = 4.0 *. (atan 1.0);;

(* Q1 *)
let volume_of_cube a =
  a *. a *. a;;

(* test *) 
volume_of_cube 3.0;;

 (*--------------------------*)

(* Q2 *)
let edgelen_of_cube volume =
	volume ** (1./.3.);;

(* test *)
edgelen_of_cube 8.;;

 (*--------------------------*)

(* Q3 *)
let volumes_of_cubes vfunc edgelens =
	List.map vfunc edgelens;;

(* test *)
let a_list = [1.; 3.; 5.];;
volumes_of_cubes volume_of_cube a_list

 (*--------------------------*)

(* Q4 *)
let rec ack (m:int) (n:int):int = 
	if m = 0 then 
	(n+1) else
	if m > 0 && n = 0 then
	(ack (m-1) 1) else
	if m > 0 && n > 0 then ack (m-1) (ack m (n-1)) else
  failwith "input error";;

(* test *)
ack 1 2;;

 (*--------------------------*)

(* Q5 *)
(* Method 1 *)
let wc words word = 
  List.length (List.filter (fun x -> x = word) words);;

(* Method 2 *)
let wc words word =
    let rec scan count words =
      match words with
      |[] -> count
      |hd :: tl -> if (compare hd word == 0) then scan (count + 1) tl
      else scan count tl
    in scan 0 words;;

  (* test *)
  let words = ["The"; "word"; "count"; "is"; "the"; "number"; "of"; "words"; "in"; "a"; "document"; "or"; "passage"; "of"; "text"];;
  wc words "of";;
  wc words "the";;
  wc (List.map String.lowercase_ascii words) "the";; 


  




















