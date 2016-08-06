(* Homework 7. testnat *)

type nat = ZERO | SUCC of nat

let a = ZERO
let b = SUCC(ZERO)
let c = SUCC(SUCC(ZERO))

let rec natadd (x, y) =
    match x with
    | ZERO -> y
    | SUCC x -> SUCC (natadd (x, y))

let x = SUCC(SUCC(ZERO))
let y = SUCC(SUCC(SUCC(ZERO)))

let _ = natadd (x, y)
  
let rec natmul (x, y) = 
    match x with
    | ZERO -> ZERO
    | SUCC ZERO -> y
    | SUCC x -> (natadd (y, natmul (x, y)))

let _ = natmul (x, y)

let rec nat2n x =
    match x with
    | ZERO -> 0
    | SUCC x -> 1 + (nat2n x)

let _ = nat2n x
let _ = nat2n y

let rec n2nat x = 
   if x = 0 then ZERO else SUCC (n2nat (x-1)) 

let a = 2 and b = 3

let _ = n2nat a
let _ = n2nat b
