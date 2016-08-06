(* Exercise 1. mathemadiga *)
(* #use "Ex1.ml";; *)

type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

exception FreeVariable

let rec subs e k =
    match e with
    | X -> k
    | INT x -> INT x 
    | REAL x -> REAL x 
    | ADD (a,b) -> ADD ((subs a k), (subs b k))
    | SUB (a,b) -> SUB ((subs a k), (subs b k))
    | MUL (a,b) -> MUL ((subs a k), (subs b k))
    | DIV (a,b) -> DIV ((subs a k), (subs b k))
    | _ -> raise FreeVariable

let incrInt e = 
    match e with
    | INT k -> INT (k + 1)
    | REAL k -> INT ((int_of_float k) + 1)
    | _ -> raise FreeVariable

let incrReal e = 
    match e with
    | INT k -> REAL ((float_of_int k) +. 1.)
    | REAL k -> REAL (k +. 1.)
    | _ -> raise FreeVariable

let rec mathemadiga e = 
    match e with
    | INT a -> float_of_int a
    | REAL a -> a

    | ADD (a, b) -> (mathemadiga a) +. (mathemadiga b)
    | SUB (a, b) -> (mathemadiga a) -. (mathemadiga b)
    | MUL (a, b) -> (mathemadiga a) *. (mathemadiga b)
    | DIV (a, b) -> (mathemadiga a) /. (mathemadiga b)
   
    | SIGMA (a, b, c) -> 
        if (mathemadiga a) > (mathemadiga b) then 0.
        else (mathemadiga (subs c a)) +. (mathemadiga (SIGMA ((incrInt a), b, c)))
    
    | INTEGRAL (a, b, c) ->
        if (mathemadiga a) > (mathemadiga b) then 0.
        else (mathemadiga (subs c a) *. 0.1) +. (mathemadiga (INTEGRAL ((incrReal a), b, c)))
    
    | _ -> raise FreeVariable


let t1 = SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1))
let test1 = mathemadiga t1

let t2 = INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X, X), INT 1))
let test2 = mathemadiga t2

