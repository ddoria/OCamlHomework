(* Homework 8-2. crazy2vla *)

type crazy2 = NIL 
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let crazy2val c =
    let rec power2 n = (* n power of 2 *)
        match n with
        | 0 -> 1
        | _ -> 2 * (power2 (n-1)) in
    let rec calc x n = (* d * 2^n *)
        match x with
        | NIL -> 0
        | ZERO x -> 0 + calc x (n+1)
        | ONE x -> 1 * (power2 n) + (calc x (n+1))
        | MONE x -> -1 * (power2 n) + (calc x (n+1))
    in calc c 0

let a = ZERO(ONE(MONE NIL));;

#trace crazy2val;;
let _ = crazy2val a;; 