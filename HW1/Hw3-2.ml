(* Homework 3- iter *)

let rec iter (n, f) i =
    if n <= 0 then i  
    else
        f (iter (n-1, f) i)  ;;

#trace iter;;
iter (3, fun x -> 2 + x) 0 ;;
