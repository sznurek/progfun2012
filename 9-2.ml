type var = string

type term = 
    | Var of var
    | Abs of var * term
    | App of term * term
    | True
    | False
    | If of term * term * term

let rec substitute v t = function
    | Var v' as var       -> if v = v' then t else var
    | App (f, x)          -> App (substitute v t f, substitute v t x)
    | Abs (v', t') as abs -> if v' = v then abs else Abs (v', substitute v t t')
    | If (a, b, c)        -> If (substitute v t a, substitute v t b, substitute v t c)
    | boolean             -> boolean

let rec nonlazy_is_value = function
    | App (Var _, x) -> nonlazy_is_value x
    | App _          -> false
    | If (x, _, _)   -> nonlazy_is_value x && x <> True && x <> False
    | _              -> true

let rec nonlazy_step = function
    | Var x as v        -> v
    | Abs _ as a        -> a
    | If (b, t, f)      -> (match b with
        | True  -> t
        | False -> f
        | _     -> If (nonlazy_step b, t, f))
    | App (f, x) as app ->
        if not (nonlazy_is_value f) then 
            App (nonlazy_step f, x)
        else if not (nonlazy_is_value x) then
            App (f, nonlazy_step x)
        else
            (match f with
                | Abs (v, t) -> substitute v x t
                | _          -> app)
    | boolean            -> boolean

let rec lazy_is_value = function
    | App (Abs _, _) -> false
    | App (x, _)     -> lazy_is_value x
    | If (x, _, _)   -> lazy_is_value x && x <> True && x <> False
    | _              -> true

let rec lazy_step = function
    | Var x as v      -> v
    | Abs _ as a      -> a
    | App (f, x) as a -> (match f with
        | Var _      -> a
        | Abs (v, t) -> substitute v x t
        | _          -> App (lazy_step f, x))
    | If (b, t, f)   -> match b with
        | True  -> t
        | False -> f
        | _     -> If (lazy_step b, t, f)

let rec eval_nonlazy t = if nonlazy_is_value t then t else eval_nonlazy (nonlazy_step t)
let rec eval_lazy t = if lazy_is_value t then t else eval_lazy (lazy_step t)

let (<$>) a b = App (a, b)
let id = Abs ("x", Var "x")
let fixpoint = let t = Abs ("x", Var "x" <$> Var "x") in t <$> t
let k = Abs ("x", Abs ("y", Var "x"))
let ubijacz = (k <$> Var "z") <$> fixpoint
let rozwalacz = (Var "z" <$> (Abs ("x", Var "x") <$> Var "y"))

let if_test = If (Abs ("x", Var "x") <$> True, False, True)
