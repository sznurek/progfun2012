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

(* ------------------ *)

type regexp = 
    | Atom of char
    | And of regexp * regexp
    | Or of regexp * regexp
    | Star of regexp

let rec match_regexp r str next fail = match r with
    | Atom c     -> if str = [] || List.hd str <> c then fail() else next (List.tl str) fail
    | And (a, b) -> match_regexp a str (fun str' fail' -> match_regexp b str' next fail') fail
    | Or (a, b)  -> match_regexp a str next (fun () -> match_regexp b str next fail)
    | Star r     -> next str (fun () -> match_regexp (And (r, Star r)) str next fail)

let run r str = match_regexp r str (fun str fail -> if str = [] then true else fail()) (fun () -> false)
