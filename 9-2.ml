type var = string

type term = 
    | Var of var
    | Abs of var * term
    | App of term * term

let rec nonlazy_is_value = function
    | App (Var _, x) -> nonlazy_is_value x
    | App _          -> false
    | _              -> true

let rec substitute v t = function
    | Var v' as var       -> if v = v' then t else var
    | App (f, x)          -> App (substitute v t f, substitute v t x)
    | Abs (v', t') as abs -> if v' = v then abs else Abs (v', substitute v t t')

let rec nonlazy_step = function
    | Var x as v        -> v
    | Abs _ as a        -> a
    | App (f, x) as app ->
        if not (nonlazy_is_value f) then 
            App (nonlazy_step f, x)
        else if not (nonlazy_is_value x) then
            App (f, nonlazy_step x)
        else
            match f with
                | Abs (v, t) -> substitute v x t
                | _          -> app

let rec lazy_is_value = function
    | App (x, _)     -> lazy_is_value x
    | App _          -> false
    | _              -> true

let rec lazy_step = function
    | Var x as v      -> v
    | Abs _ as a      -> a
    | App (f, x) as a -> match f with
        | Var _      -> a
        | Abs (v, t) -> substitute v x t
        | _          -> App (lazy_step f, x)

let rec eval_nonlazy t = if nonlazy_is_value t then t else eval_nonlazy (nonlazy_step t)
let rec eval_lazy t = if lazy_is_value t then t else eval_lazy (lazy_step t)

let (<$>) a b = App (a, b)
let id = Abs ("x", Var "x")
let fixpoint = let t = Abs ("x", Var "x" <$> Var "x") in t <$> t
let k = Abs ("x", Abs ("y", Var "x"))
let ubijacz = (k <$> Var "z") <$> fixpoint
let rozwalacz = (Var "z" <$> (Abs ("x", Var "x") <$> Var "y"))
