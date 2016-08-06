(* Homework 1-5 *)

type mylist = Nil | Cons of (int * mylist)

let rec for_all f l =
    match l with
    | Nil -> true
    | Cons(x, t) -> (f x) && (for_all f t) ;;