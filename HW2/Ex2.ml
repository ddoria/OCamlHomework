(* Exercise 2. maketourn *)
(* #use "Ex2.ml";; *)

type team = SouthKorea | NorthKorea | Australia | Japan | NewZealand
          | Algeria | Cameroon | CoteDlvoire | Ghana | Nigeria | SouthAfrica
          | Honduras | Mexico | USA
          | Argentina | Brazil | Chile | Paraguay | Uruguay
          | Denmark | England | France | Germany | Greece | Italy | Spain
          | Netherlands | Portugal | Serbia | Slovakia | Slovenia | Switzerland

type tourna = LEAF of team
            | NODE of tourna * tourna
        
type continent = Asia | Africa | NorthAmerica | SouthAmerica | Europe

let whereIsTeam t =
    match t with
    | SouthKorea | NorthKorea | Australia | Japan | NewZealand -> Asia          
    | Algeria | Cameroon | CoteDlvoire | Ghana | Nigeria | SouthAfrica -> Africa
    | Honduras | Mexico | USA -> NorthAmerica
    | Argentina | Brazil | Chile | Paraguay | Uruguay -> SouthAmerica
    | Denmark | England | France | Germany | Greece | Italy | Spain 
    | Netherlands | Portugal | Serbia | Slovakia | Slovenia | Switzerland -> Europe 
    
exception EmptyList

let rec maketourn tl =
    match tl with  
    | [] -> raise EmptyList      
    | t1::t2::[] -> NODE(LEAF t1, LEAF t2)
    | t1::[] -> LEAF t1 
    | t1::t2::tl' -> 
        if((whereIsTeam t1) <> (whereIsTeam t2)) then NODE(NODE(LEAF t1, LEAF t2), (maketourn tl'))
        else NODE(maketourn (t1::tl'), LEAF t2) ;;

let t1 = [SouthKorea; NorthKorea; Brazil; Italy; Portugal; Japan]
let tA = maketourn t1

