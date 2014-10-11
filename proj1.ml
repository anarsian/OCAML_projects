(* CSE 130: Programming Assignment 1
 * misc.ml
 * Name: Anish Narsian
 * PID: A98071125
*)

(* sumList : int list -> int
 *sums a list of nums
*)
let rec sumList xs = match xs with
  | []    ->    0
  | x::xs' -> x + sumList xs'
;;

(*uncomment and run AFTER you have implemented sumList*)

let _ = sumList [1; 2; 3; 4]
let _ = sumList [1; -2; 3; 5]
let _ = sumList [1; 3; 5; 7; 9; 11]





(*
*reverses a list
*)
let rec listReverse l =
  let rec recHelper l1 l= match l with
    |[] -> l1
    |x::xs' -> recHelper(x::l1) xs' in
    recHelper [] l;;


(* uncomment and run AFTER you have implemented listReverse*)

let _ = listReverse [1; 2; 3; 4]
let _ = listReverse ["a"; "b"; "c"; "d"]


(* `digitsOfInt n` should return `[]` if `n` is not positive,
   and otherwise returns the list of digits of `n` in the 
   order in which they appear in `n`. *)

(* digitsOfInt : int -> int list  *)
let rec digitsOfInt n = 
  let rec recRev n1 n = match n<1 with
    |true -> n1
    |false -> recRev(n mod 10 :: n1) (n/10) in
    recRev [] n;;


(* uncomment and run AFTER you have implemented digitsOfInt *)

let _ = digitsOfInt 3124
let _ = digitsOfInt 352663




(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
*)

let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *
 * NOTE: assume that additivePersistence is only called with positive numbers
*)


let rec additivePersistence n = begin
  let j = sumList(digits n) in
    if j > 9 then
      1 + additivePersistence j
    else
      1
end;;



(* uncomment and run AFTER you have implemented additivePersistence  *)

let _ = additivePersistence 9876



(* NOTE: assume that digitalRoot is only called with positive numbers
 *Return digital root which is the base 
 * that the number gets to using additivePersistence method
*)

let rec digitalRoot n = begin
  let j = sumList( digits n) in
    if(j > 9) then
      digitalRoot j
    else
      j
end;;


(* uncomment and run AFTER you have implemented digitalRoot *)

let _ = digitalRoot 9876







(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
*)
let explode s = 
  let rec go i = 
    if i >= String.length s 
    then [] 
    else (s.[i]) :: (go (i+1)) 
  in
    go 0


(*
*Returns true if word is a palindrome, false if not
*)

let palindrome w = 
  let x = explode w in
    if(x = listReverse x)
    then true
    else
      false
;;




(* uncomment and run AFTER you have implemented digitalRoot *)

let _ = palindrome "malayalam"
let _ = palindrome "myxomatosis"























(*130*************************************************************)
(*130**** DO NOT MODIFY ANY CODE AFTER THIS **********************)
(*130*************************************************************)

type test = unit -> string

let key        = ""     (* change *)
let prefix130  = "130"  (* change *)

let print130 s = print_string (prefix130^">>"^s)

exception ErrorCode of string

exception TestException

type result = Pass | Fail | ErrorCode of string

let score = ref 0
let max = ref 0
let timeout = 300

let runWTimeout (f,arg,out,time) = 
  try if compare (f arg) out = 0 then Pass else Fail
  with e -> (print130 ("Uncaught Exception: "^(Printexc.to_string e)); ErrorCode "exception") 

let testTest () =
  let testGood x = 1 in
  let testBad x = 0 in 
  let testException x = raise TestException in
  let rec testTimeout x = testTimeout x in
    runWTimeout(testGood,0,1,5) = Pass &&  
    runWTimeout(testBad,0,1,5) = Fail &&  
    runWTimeout(testException,0,1,5) = ErrorCode "exception" && 
    runWTimeout(testTimeout,0,1,5) = ErrorCode "timeout"

let runTest ((f,arg,out),points,name) =
  let _   = max := !max + points in
  let outs = 
    match runWTimeout(f,arg,out,timeout) with 
        Pass -> (score := !score + points; "[pass]")
      | Fail -> "[fail]"
      | ErrorCode e -> "[error: "^e^"]"  in
    name^" "^outs^" ("^(string_of_int points)^")\n"

let mkTest f x y name = runTest ((f, x, y), 1, name)

let badTest () = "WARNING: Your tests are not valid!!\n"

let scoreMsg () = 
  Printf.sprintf "Results: Score/Max = %d / %d \n" !score !max 

let doTest f = 
  try f () with ex -> 
    Printf.sprintf "WARNING: INVALID TEST THROWS EXCEPTION!!: %s \n\n"
      (Printexc.to_string ex)

(*130*************************************************************)
(*130************** Sample Tests *********************************)
(*130*************************************************************)

let sampleTests =
  [
    (fun () -> mkTest
                 sumList
                 [1;2;3;4]
                 10
                 "sample: sumList 1"
    );
    (fun () -> mkTest 
                 sumList 
                 [1;-2;3;5] 
                 7 
                 "sample: sumList 2"
    ); 
    (fun () -> mkTest 
                 sumList 
                 [1;3;5;7;9;11]
                 36 
                 "sample: sumList 3"
    ); 
    (fun () -> mkTest 
                 digitsOfInt 
                 3124 
                 [3;1;2;4] 
                 "sample: digitsOfInt 1"
    ); 
    (fun () -> mkTest 
                 digitsOfInt 
                 352663 
                 [3;5;2;6;6;3] 
                 "sample: digitsOfInt 2"
    ); 
    (fun () -> mkTest 
                 digits
                 31243
                 [3;1;2;4;3] 
                 "sample: digits 1"
    ); 
    (fun () -> mkTest 
                 digits
                 (-23422)
                 [2;3;4;2;2]
                 "sample: digits 2"
    ); 
    (fun () -> mkTest 
                 additivePersistence 
                 9876 
                 2 
                 "sample: additivePersistence1"
    ); 
    (fun () -> mkTest 
                 digitalRoot 
                 9876 
                 3 
                 "sample: digitalRoot"
    ); 
    (fun () -> mkTest 
                 listReverse
                 [1;2;3;4] 
                 [4;3;2;1]
                 "sample: reverse 1"
    ); 
    (fun () -> mkTest 
                 listReverse 
                 ["a";"b";"c";"d"]
                 ["d";"c";"b";"a"] 
                 "sample: rev 2"
    ); 
    (fun () -> mkTest 
                 palindrome 
                 "malayalam" 
                 true
                 "sample: palindrome 1"
    ); 
    (fun () -> mkTest 
                 palindrome 
                 "myxomatosis" 
                 false
                 "sample: palindrome 2"
    )] 

let _ =
  let report = List.map doTest (sampleTests) in
  let _ = List.iter print130 (report@([scoreMsg()])) in
  let _ = print130 ("Compiled\n") in
    (!score, !max)
