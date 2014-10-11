exception MLFailure of string

type binop = 
      Plus 
    | Minus 
    | Mul 
    | Div 
    | Eq 
    | Ne 
    | Lt 
    | Le 
    | And 
    | Or          
    | Cons

type expr =   
      Const of int 
    | True   
    | False      
    | NilExpr
    | Var of string    
    | Bin of expr * binop * expr 
    | If  of expr * expr * expr
    | Let of string * expr * expr 
    | App of expr * expr 
    | Fun of string * expr    
    | Letrec of string * expr * expr

type value =  
      Int of int		
    | Bool of bool          
    | Closure of env * string option * string * expr 
    | Nil                    
    | Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
      Int i -> 
        Printf.sprintf "%d" i
    | Bool b -> 
        Printf.sprintf "%b" b
    | Closure (evn,fo,x,e) -> 
        let fs = match fo with None -> "Anon" | Some fs -> fs in
          Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
    | Pair (v1,v2) -> 
        Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
    | Nil -> 
        "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
    "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
          (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
          (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
          x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
          x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
                | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

let lookup (x,evn) = 
  let temp = listAssoc (x,evn) in
    match temp with
      | Some x -> x
      | None -> raise (MLFailure "not found")


let rec eval (evn,e) = 
  match e with
    |Const a -> Int a
    |True -> Bool true
    |False -> Bool false
    |Var a -> lookup(a, evn)
    |Bin(e1, a, e2) -> let e11 = eval(evn, e1) in
        let e21 = eval(evn, e2) in
          begin 
            match (e11, e21) with
              |(Bool x, Bool y) -> 
                  begin
                    match a with
                      | Or -> Bool (x || y)
                      |And -> Bool (x && y)
                      |Eq  -> Bool (x = y)
                      |Ne -> Bool (x != y)
                      |_  -> raise( MLFailure "undefind operation")
                  end
              |(Int x, Int y) -> 
                  begin 
                    match a with
                      |Plus -> Int (x + y)
                      |Minus -> Int (x - y)
                      |Mul -> Int (x*y)
                      |Div -> begin
                          match y with 
                            |0 -> raise( MLFailure "Error: Dividing by zero")
                            |_ -> Int (x/y) 
                        end
                      |Le -> Bool (x <= y)
                      |Lt -> Bool (x < y)
                      |Eq -> Bool (x = y)
                      |Ne -> Bool (x != y)
                      |_ -> raise( MLFailure "undefined operation")
                  end
              |_ -> raise(MLFailure "Unexpected operands")
          end
    |If(e1, e2, e3) -> 
        let temp = eval(evn, e1) in
          (match temp with
            | Bool p -> if(p) then eval(evn, e2) else eval(evn, e3)
            |_ -> raise(MLFailure "incorrect type passed in e1"))
    (*|Fun(x, y) -> eval(evn, y)*)
    |Let(x, e1, e2) -> let temp = (x, eval(evn, e1)) :: evn in
          eval(temp, e2)
    |Letrec(x, e1, e2) ->

        let temp = 
          begin 
            match eval(evn, e1) with
              |Closure(nevn, j, k,e) -> 
                  (match j with
                      Some x -> 
                        Closure(nevn, Some x, k, e)
                    |None ->  Closure(nevn, None, k, e)
                  )(*
|Closure(nevn, None, k, e) -> Closure(nevn, None, k, e)*)

              |  _  -> eval(evn, e1) 
          end in
          eval(((x,temp)::evn), e2)


    |Fun(x,e) -> Closure(evn, None, x,e)
    |App(e1,e2) -> 
        let e11 = eval(evn, e1) in 
        let e21 = eval(evn, e2) in
          begin
            match e11 with
                Closure(nevn, None, x,e) ->
                  eval((x, e21)::(List.append nevn evn), e)
              | Closure(nevn, Some k, x,e) as f->
                  eval((k,e21)::(x,f)::(List.append nevn evn), e)
          end
    | _ -> raise (MLFailure("Incorrect Operation"))
;;










(**********************     Testing Code  ******************************)

(* Uncomment to test part (a)

   let evn = [("z1",Int 0);("x",Int 1);("y",Int 2);("z",Int 3);("z1",Int 4)]

   let e1  = Bin(Bin(Var "x",Plus,Var "y"), Minus, Bin(Var "z",Plus,Var "z1"))

   let _   = eval (evn, e1)        (* EXPECTED: Nano.value = Int 0 *)

   let _   = eval (evn, Var "p")   (* EXPECTED:  Exception: Nano.MLFailure "variable not bound: p". *)

*)

(* Uncomment to test part (b) 

   let evn = [("z1",Int 0);("x",Int 1);("y",Int 2);("z",Int 3);("z1",Int 4)]

   let e1  = If(Bin(Var "z1",Lt,Var "x"),Bin(Var "y",Ne,Var "z"),False)

   let _   = eval (evn,e1)         (* EXPECTED: Nano.value = Bool true *)

   let e2  = If(Bin(Var "z1",Eq,Var "x"), 
   Bin(Var "y",Le,Var "z"),
   Bin(Var "z",Le,Var "y")
   )

   let _   = eval (evn,e2)         (* EXPECTED: Nano.value = Bool false *)
*)


(* Uncomment to test part (c) 

   let e1 = Bin(Var "x",Plus,Var "y")

   let e2 = Let("x",Const 1, Let("y", Const 2, e1)) 

   let _  = eval ([], e2)          (* EXPECTED: Nano.value = Int 3 *)

   let e3 = Let("x", Const 1, 
   Let("y", Const 2, 
   Let("z", e1, 
   Let("x", Bin(Var "x",Plus,Var "z"), 
   e1)
   )
   )
   )

   let _  = eval ([],e3)           (* EXPCETED: Nano.value = Int 6 *)

*)


(* Uncomment to test part (d) *)

let _ = eval ([], Fun ("x",Bin(Var "x",Plus,Var "x"))) 

(* EXPECTED: Nano.value = Closure ([], None, "x", Bin (Var "x", Plus, Var "x")) *)

let _ = eval ([],App(Fun ("x",Bin(Var "x",Plus,Var "x")),Const 3));;

(* EXPECTED: Nano.value = Int 6 *)

let e3 = Let ("h", Fun("y", Bin(Var "x", Plus, Var "y")), 
              App(Var "f",Var "h"))

let e2 = Let("x", Const 100, e3)

let e1 = Let("f",Fun("g",Let("x",Const 0,App(Var "g",Const 2))),e2) 

let _  = eval ([], e1)        
(* EXPECTED: Nano.value = Int 102 *)

let _ = eval ([],Letrec("f",Fun("x",Const 0),Var "f"))
(* EXPECTED: Nano.value = Closure ([], Some "f", "x", Const 0) *)



(* Uncomment to test part (e)*)

let _ = eval ([], 
              Letrec("fac", 
                     Fun("n", If (Bin (Var "n", Eq, Const 0), 
                                  Const 1, 
                                  Bin(Var "n", Mul, App(Var "fac",Bin(Var "n",Minus,Const 1))))),
                     App(Var "fac", Const 10)))

(* EXPECTED: Nano.value = Int 3628800 *)



(* Uncomment to test part (f)

   let _ = eval ([],Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr)))

   (* EXPECTED: Nano.value = Pair (Int 1, Pair (Int 2, Nil)) *)

   let _ = eval ([],App(Var "hd",Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr))))

   (* EXPECTED: Nano.value = Int 1 *)

   let _ = eval ([],App(Var "tl",Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr))))

   (* EXPECTED: Nano.value = Pair (Int 2, Nil) *)

*)
