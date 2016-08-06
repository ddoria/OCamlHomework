(* Homework 5. meetin *)

type team = Korea 
          | France 
          | Usa 
          | Brazil 
          | Japan 
          | Nigeria 
          | Cameroon
          | Poland 
          | Portugal
          | Italy 
          | Germany 
          | Sweden 
          | England
          | Croatia 
          | Argentina

type tourna = LEAF of team 
            | NODE of tourna * tourna

exception Error_Input

let rec meetin tona (teamA, teamB) =
    let rec member_of tona =
        match tona with
        | LEAF team -> team = teamA || team = teamB
        | NODE (tonaA, tonaB) -> member_of tonaA || member_of tonaB
    in
    let rec match_depth tona = 
        match tona with
        | LEAF _ -> 1
        | NODE (tonaA, tonaB) ->
            if member_of tonaA then
                if member_of tonaB then 0
                else (match_depth tonaA) + 1
            else
                if member_of tonaB then
                    (match_depth tonaB) + 1
                else raise Error_Input
    in
    let depth = match_depth tona
    in
    let rec find_until tona depth =
        match tona with
        | LEAF _ -> 1
        | NODE (tonaA, tonaB) ->
            if depth = 0 then 2
            else
                (find_until tonaA (depth - 1)) + (find_until tonaB (depth - 3))
    in
    let result = find_until tona depth 

   
