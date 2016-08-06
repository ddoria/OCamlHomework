(* Homework 6. eval *)

type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
    and expr = NUM of int
             | PLUS of expr * expr
             | MINUS of expr * expr

let rec exprVal e =
    match e with
        | NUM n -> n
        | PLUS (n1, n2) -> (exprVal n1) + (exprVal n2)
        | MINUS (n1, n2) -> (exprVal n1) - (exprVal n2)

let rec eval form =
    match form with
        | TRUE -> true
        | FALSE -> false
        | NOT formA -> not (eval formA) 
        | ANDALSO (formA, formB) -> (eval formA) && (eval formB)
        | ORELSE (formA, formB) -> (eval formA) || (eval formB) 
        | IMPLY (formA, formB) -> not (eval formA) || (eval formB)
        | LESS (exA, exB) -> (exprVal exA) < (exprVal exB) ;; 
