(* Exercise 3. pptree *)
(* #use "Ex3.ml";; *)

type team = SouthKorea | NorthKorea | Australia | Japan | NewZealand
          | Algeria | Cameroon | CoteDlvoire | Ghana | Nigeria | SouthAfrica
          | Honduras | Mexico | USA
          | Argentina | Brazil | Chile | Paraguay | Uruguay
          | Denmark | England | France | Germany | Greece | Italy | Spain
          | Netherlands | Portugal | Serbia | Slovakia | Slovenia | Switzerland

type tourna = LEAF of team
            | NODE of tourna * tourna
        
let stringf_of_team t = 
    match t with
    | SouthKorea -> "S"
    | NorthKorea -> "N"
    | Australia -> "A"
    | Japan -> "J"
    | NewZealand -> "N"
    | Algeria -> "A"
    | Cameroon -> "C"
    | CoteDlvoire -> "C"
    | Ghana -> "G"
    | Nigeria -> "N"
    | SouthAfrica -> "S"
    | Honduras -> "H" 
    | Mexico -> "M"
    | USA -> "U"
    | Argentina -> "A"
    | Brazil -> "B"
    | Chile -> "C"
    | Paraguay -> "P" 
    | Uruguay -> "U"
    | Denmark ->"D"
    | England -> "E"
    | France -> "F"
    | Germany -> "G" 
    | Greece -> "G"
    | Italy -> "I"
    | Spain -> "iS"
    | Netherlands -> "N"
    | Portugal -> "P" 
    | Serbia -> "S"
    | Slovakia -> "S"
    | Slovenia -> "S" 
    | Switzerland -> "S"

let rec depthTree t = 
    match t with 
    | LEAF a -> 0
    | NODE (l,r) -> 
        let l = 1 + depthTree l 
        in let r = 1 + depthTree r
        in if (l > r) then l else r

let count d = 
    if d = 0 then 0 else 2*d-1

let rec position d =
    if d = 0 then 0
    else if d = 1 then 2
    else d + (position (d-1))

let rec pptree t = 
    let rec draw t =
        let d = depthTree t 
        in let c = count d
        in let p = position d 
        in
            match t with
            | LEAF k -> (stringf_of_team k) ^ "\n"
            | NODE(l,r) -> 
                let str = (String.make (p-1) ' ') ^ "|"
                in 
                    match (l,r) with
                    | (LEAF l, LEAF r) ->
                        str ^ "\n" ^ (String.make (p-2) ' ') ^ (stringf_of_team l) ^ "-" ^ (stringf_of_team r)
                    | (l, LEAF r) ->
                        str ^ (String.make (p-2) ' ') ^ (String.make c '-') ^ (stringf_of_team r) ^ "\n" ^ (draw l)
                    | (l, r) ->
                        str ^ (String.make (p-2) ' ') ^ (String.make c '-') ^ str ^ "\n" ^ (draw l) ^ (draw r)
    in (print_string (draw t)) 

let t1 = (LEAF Portugal)
let t2 = (NODE(LEAF Portugal, LEAF Slovakia))

let test1 = pptree t1
let test2 = pptree t2