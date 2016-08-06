(* Exercise 4. diff *)
(* #use "Ex4.ml";; *)

type ae = CONST of int 
        | VAR of string
        | POWER of string * int 
        | TIMES of ae list
        | SUM of ae list

exception ErrorInput

let rec diff (ae, str) =
    match ae with
    | CONST n -> CONST 0
    | VAR s -> 
        if s = str then CONST 1 else CONST 0
    | POWER(s, n) ->
        if s = str then TIMES([(CONST n); POWER(s, n-1)]) else CONST 0
    | TIMES al ->
        begin 
            match al with
            | [] -> TIMES([])
            | k::al' -> TIMES([diff(k, str); TIMES al'])
        end
    | SUM al ->
        begin
            match al with
            | [] -> SUM([])
            | k::al' -> SUM([diff(k, str); diff(SUM al', str)])
        end

let t1 = SUM([TIMES[VAR "a"; POWER("x", 2)]; TIMES[VAR "b"; VAR "x"]; VAR "c"])
let test1 = diff(t1, "x")