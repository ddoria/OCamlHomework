(* Homework 1-2 *)

type mylist = Nil | Cons of (int * mylist)

let f = fun x -> x+1

let rec map f l =
    match l with
    | Nil -> Nil 
    | Cons (x, t) -> Cons (f x, map f t) 
