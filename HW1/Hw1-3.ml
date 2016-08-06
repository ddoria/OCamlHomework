(* Homework 1-3 *)

type mylist = Nil | Cons of (int * mylist)

let rec fold_left f i l = 
    match l with
    | Nil -> 0
    | Cons (x, t) -> f fold_left f i x 