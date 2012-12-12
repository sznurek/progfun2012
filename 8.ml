type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree
type 'a cont  = Cont of (unit -> ('a * 'a cont) option)

let rec flatten = function
    | Leaf a     -> [a]
    | Node (l,r) -> (flatten l) @ (flatten r)

let samefringe' a b = (flatten a) = (flatten b)

let get_next tree = 
    let rec aux tree cont = match tree with
        | Leaf a      -> Some (a, cont)
        | Node (l, r) -> aux l (Cont (fun () -> aux r cont))
    in aux tree (Cont (fun () -> None))

let runcont (Cont f) = f()

let samefringe a b = 
    let rec aux c next = function
        | Leaf a      -> (match runcont c with
            | Some (b, c') -> if a <> b then false else next c'
            | None         -> false)
        | Node (l, r) -> aux c (fun c -> aux c next r) l
    in aux (Cont (fun () -> get_next b)) (fun _ -> true) a

let ex1 = Node (Node (Leaf 1, Leaf 2), Leaf 3)
let ex2 = Node (Leaf 1, Node (Leaf 2, Leaf 3))
let ex3 = Node (Node (Leaf 1, Leaf 3), Leaf 2)
