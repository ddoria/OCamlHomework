(* Homework 1-4 *)

type mylist = Nil | Cons of (int * mylist)

let rec mem i l = 
    match l with
    | Nil -> false
    | Cons (x, t) -> (x = i) || (mem i t) ;;
    
mem 0 (Cons(1, Cons(2, Nil))) ;;