type var = string

type term = 
    | Var of var
    | Abs of var * term
    | App of term * term

let is_value = function
    | App _ -> false
    | _     -> true

let rec substitute v t = function
    | Var v' as var       -> if v = v' then t else var
    | App (f, x)          -> App (substitute v t f, substitute v t x)
    | Abs (v', t') as abs -> if v' = v then abs else Abs (v', substitute v t t')

let rec nonlazy_step = function
    | Var x as v -> v
    | Abs _ as a -> a
    | App (f, x) ->
        if not (is_value f) then 
            App (nonlazy_step f, x)
        else if not (is_value x) then
            App (f, nonlazy_step x)
        else
            match f with
                | Abs (v, t) -> substitute v x t
                | _          -> failwith "Application of var!"

let rec lazy_step = function
    | Var x as v -> v
    | Abs _ as a -> a
    | App (f, x) -> match f with
        | Abs (v, t) -> substitute v x t
        | _          -> App (lazy_step f, x)

let rec eval step t = if is_value t then t else eval step (step t)
let eval_nonlazy = eval nonlazy_step
let eval_lazy = eval lazy_step

let (<$>) a b = App (a, b)
let fixpoint = let t = Abs ("x", Var "x" <$> Var "x") in t <$> t
let k = Abs ("x", Abs ("y", Var "x"))
let ubijacz = (k <$> Var "z") <$> fixpoint

