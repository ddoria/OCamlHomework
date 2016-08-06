(* Homework 3. iter *)

let identi f = f

let rec iter (n, fun x) =
    if (n <= 0) then (identi f x)
    else iter (n-1, f (f x));;

iter (2, function x -> 2+x) 0 ;;

