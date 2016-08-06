(* 1. mylist의 length 함수 정의 *)

type mylist = Nil | Cons of (int * mylist)

(*1-1*)
let rec length l =
    match l with
    | Nil -> 0
    | Cons (k, t) -> 1 + length t ;;

(*1-2*)
let rec map f l =
    match l with
    | Nil -> Nil 
    | Cons (x, t) -> Cons (f x, map f t) ;;

map (fun x -> x+1) (Cons(1, Cons(2, Nil))) ;;    

(*1-3*)
let rec fold_left f i l = 
    match l with
    | Nil -> 0
    | Cons (x, t) -> f (fold_left f i t) x ;;

fold_left (fun x y -> x + y) 0 (Cons(1, Cons(2, Cons(3, Nil)))) ;;    

(*1-4*)
let rec mem i l = 
    match l with
    | Nil -> false
    | Cons (x, t) -> 
        if x=i then true
        else mem i t ;;

mem 0 (Cons(1, Cons(2, Nil))) ;;

(*1-5*)
let rec for_all f l =
    match l with
    | Nil -> true
    | Cons(x, t) -> 
        if f x then for_all f t 
        else false ;;

for_all (fun x -> x > 0) (Cons(1, Cons(2, Nil))) ;;