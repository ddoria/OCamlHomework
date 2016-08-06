(* Homework 4. parenize *)

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

let string_of_team t =
    match t with
    | Korea     -> "Korea"
    | France    -> "France"
    | Usa       -> "Usa" 
    | Brazil    -> "Brazil"
    | Japan     -> "Japan"
    | Nigeria   -> "Nigeria" 
    | Cameroon  -> "Cameroon"
    | Poland    -> "Poland" 
    | Portugal  -> "Portugal"
    | Italy     -> "Italy"
    | Germany   -> "Germany"
    | Sweden    -> "Sweden"
    | England   -> "England"
    | Croatia   -> "Croatia" 
    | Argentina -> "Argentina"

let rec parenize tona =
    match tona with
    | LEAF t -> string_of_team t
    | NODE (toA, toB) -> "(" ^ parenize toA ^ " " ^ parenize toB ^ ")" ;;

parenize (NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)) ;;