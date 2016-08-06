(* Homework 9. crazy2add *)

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

let rec crazy2add (x, y) =
    match (x, y) with
    | (NIL, NIL) -> NIL
    | (x, NIL) -> x
    | (NIL, y) -> y 
    | (ZERO x, ZERO y)
    | (ONE x, MONE y)
    | (MONE x, ONE y) -> ZERO (crazy2add (x, y))
    | (ZERO x, ONE y)
    | (ONE x, ZERO y) -> ONE (crazy2add (x, y))
    | (MONE x, ZERO y) 
    | (ZERO x, MONE y) -> MONE (crazy2add (x, y))
    | (ONE x, ONE y) ->
        let carry_one = crazy2add (ONE NIL, x)
        in ZERO (crazy2add (carry_one, y))
    | (MONE x, MONE y) ->
        let carry_mone = crazy2add (MONE NIL, x)
        in ZERO (crazy2add (carry_mone, y))

