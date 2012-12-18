type var = string

type aexp =
    | Num of int
    | Var of var
    | Plus of aexp * aexp
    | Mult of aexp * aexp

type bexp = 
    | True
    | False
    | Not of bexp
    | And of bexp * bexp
    | Or of bexp * bexp
    | Eq of aexp * aexp

type cmd = 
    | Assign of var * aexp
    | Skip
    | Seq of cmd * cmd
    | If of bexp * cmd * cmd
    | For of var * aexp * aexp * cmd
    | While of bexp * cmd
    | Repeat of var * int * int * cmd * mem (* takie tam, pomocnicze *)
    | Debug of string

type mem = var -> int

let empty_mem = fun _ -> failwith "Cannot find variable."
let lookup mem var = mem var
let update mem var value = function v -> if v = var then value else mem v

let rec aeval mem = function
    | Num n       -> n
    | Var v       -> mem v
    | Plus (a, b) -> (aeval mem a) + (aeval mem b)
    | Mult (a, b) -> (aeval mem a) * (aeval mem b)

let rec beval mem = function
    | True       -> true
    | False      -> false
    | Not b      -> not (beval mem b)
    | And (a, b) -> (beval mem a) && (beval mem b)
    | Or  (a, b) -> (beval mem a) || (beval mem b)
    | Eq  (a, b) -> (aeval mem a) = (aeval mem b)

let rec eval mem = function
    | Assign (v, a)          -> update mem v (aeval mem a)
    | Skip                   -> mem
    | Seq (a, b)             -> let mem' = eval mem a in eval mem' b
    | If (b, t, f)           -> if (beval mem b) then eval mem t else eval mem f
    | While (b, c) as w      -> if (beval mem b) then eval mem (Seq (c, w)) else mem
    | For (v, f, t, c)       -> let a1 = aeval mem f in let a2 = aeval mem t in eval mem (Repeat (v,a1, a2, c, mem))
    | Repeat (v, f, t, c, m) -> if t > f then mem else let m' = eval (update m v f) c in eval m' (Repeat (v, f + 1, t, c, m))
    | Debug s                -> print_string s; print_string "\n"; mem

let run (prog, arith) = let mem = eval empty_mem prog in aeval mem arith

let (<>) a b = Seq (a, b)
let factorial = Assign ("n", Num 5) <> 
                Assign ("r", Num 1) <>
                While (Not (Eq (Var "n", Num 1)), 
                    Assign ("r", Mult (Var "n", Var "r")) <>
                    Assign ("n", Plus (Var "n", Num (-1))))
