(* Homework 2-2. sigma *)

let rec sigma (a, b, f) =
    if a > b then 0 else (f a) + (sigma ((a+1), b, f)) ;;

#trace sigma;;
sigma (1, 3, (fun x -> x+1)) ;;