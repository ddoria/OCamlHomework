(* Homework 1-1 *)

type mylist = Nil | Cons of (int * mylist)

let rec length l =
    match l with
    | Nil -> 0
    | Cons (k, t) -> 1 + length t