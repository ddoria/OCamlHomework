(* Homework 8. crazy2vla *)

type crazy2 = NIL 
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec power a b =
    if (b=0) then 1
    else if (b=1) then a
    else if (b>1) then a * (power a (b-1))
    else 0 ;;

power 2 3;;
power 2 1;;

let rec sigma c n =
    if n > 0 then (c * n) + (sigma c (n-1)) else 0;; 

sigma 2 3;;

let rec searchK c =
    match c with
        | NIL -> -1
        | ZERO c' -> 1 + searchK c'
        | ONE c'-> 1 + searchK c'
        | MONE c' -> 1 + searchK c' ;;

searchK (ZERO(ONE(MONE NIL))) ;;
let cc = (ZERO(ONE(MONE NIL))) ;;
(*let k = searchK cc;;
let m = power 2 k;;
*)
let rec crazy2val crz =
    match crz with
        | NIL -> 1
        | ZERO NIL -> 0 * (power 2 (searchK crz))
        | ONE NIL -> 1 * (power 2 (searchK crz))
        | MONE NIL -> -1 * (power 2 (searchK crz))
        | ZERO c -> 0 + (crazy2val c)
        | ONE c -> 1 + (crazy2val c)
        | MONE c -> -1 + (crazy2val c);;

let rec crazyVal c v =
    match c with 
        | ZERO NIL -> 0 * (power 2 1) + v
        | ONE NIL -> 1 * (power 2 1) + v
        | MONE NIL -> -1 * (power 2 1) + v ;;

crazyVal (ONE 5);;        

#trace crazy2val;;


crazy2val cc ;;


         
        