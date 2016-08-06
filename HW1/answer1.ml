open Printf
  
(*homework1.ml*)

let _ = printf "\n===== Excercise. 1.1 =====\n"

type mylist = Nil | Cons of (int * mylist)
			      
let rec print_mylist lst =
  match lst with
  | Nil -> print_string "\n"
  | Cons(n, lst2) -> print_int n ; print_string " " ; print_mylist lst2
								   
let rec length lst =  
  match lst with
  | Nil -> 0
  | Cons (n, lst2) -> (length lst2) + 1

let mList = Cons(3, Cons(2,Nil))

let _ = print_string "myList : " ; print_mylist mList
let _ = printf "length : %d\n" (length mList)


	       
let _ = printf "\n===== Excercise. 1.2 =====\n"

let rec map (f : int -> int) lst = 
  match lst with
  | Nil -> Nil
  | Cons (x, lst2) -> Cons((f x), (map f lst2))
			  
let mList2 = map (fun x -> x+1) mList
let _ = print_string "myList : " ; print_mylist mList
let _ = print_string "myList : " ; print_mylist mList2



let _ = printf "\n===== Excercise. 1.3 =====\n"

let rec fold_left (f : int -> int -> int ) z lst =
  match lst with
  | Nil -> z
  | Cons(x, lst2) -> (fold_left f (f z x) lst2)

let foldResult = fold_left (fun x y -> x + y) 0 (mList)
let _ = print_string "myList : " ; print_mylist mList
let _ = printf "foldResult : %d\n" foldResult



let _ = printf "\n===== Excercise. 1.4 =====\n"

let rec mem z lst =
  match lst with
  | Nil -> false
  (* | Cons(x, lst2) -> (if x = z then true else (mem z lst2)) *)
  | Cons(x, lst2) -> (x = z) || (mem z lst2)  

		       
let _ = print_string "myList : " ; print_mylist mList
let _ = printf "mem 1 : %B\n" (mem 1 mList)
let _ = printf "mem 2 : %B\n" (mem 2 mList)



let _ = printf "\n===== Excercise. 1.5 =====\n"

let rec for_all (f : int -> bool) lst =
  match lst with
  | Nil -> true
  | Cons(x, lst2) -> (f x) && (for_all f lst2)

let _ = print_string "myList : " ; print_mylist mList
let _ = printf "for_all x>2 : %B\n" (for_all (fun x -> x > 2) mList)
let _ = printf "for_all x>1 : %B\n" (for_all (fun x -> x > 1) mList)



let _ = printf "\n===== Excercise. 2 =====\n"

let rec sigma a b (f:int->int) = 
  if a > b then 0 else (f a) + (sigma (a+1) b f)

let _ = printf "sigma 1 3 (fun x->x+1) : %d\n" (sigma 1 3 (fun x->x+1))



let _ = printf "\n===== Excercise. 3 =====\n"

let rec iter (n,(f: int -> int)) z =
  if 0 >= n then 0 else (f z) + (iter((n-1),f) z );;
  
let _ = printf "iter(5,(fun x->2+x)) 0 : %d\n" (iter(5,(fun x->2+x)) 0)



let _ = printf "\n===== Excercise. 4 =====\n"

type team =
  | Korea
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
      
type tourna = LEAF of team | NODE of tourna * tourna
						
let string_of_team team = 
  match team with
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

let rec parenize tourna = 
  match tourna with
  | LEAF t -> string_of_team t
  | NODE (n1,n2) -> "("^(parenize n1)^" "^(parenize n2)^")"
							
let tourna01 = NODE (LEAF Korea, LEAF Portugal) 
let tourna02 = NODE (LEAF Brazil, LEAF Italy) 
let tourna03 = NODE (tourna01, tourna02)
let tourna04 = NODE (LEAF Sweden, tourna03)
let tourna05 = NODE (LEAF England, LEAF Croatia)
let tourna06 = NODE (tourna04, tourna05)

let _ = printf "%s\n" (parenize(tourna06))


let _ = printf "\n===== Excercise. 5 =====\n"

exception Error_Input

let meetin (tona: tourna) (team1, team2) =
  let rec member_of (tona: tourna) =
    match tona with
    | LEAF team -> team = team1 || team = team2
    | NODE (tona1, tona2) -> member_of tona1 || member_of tona2
  in
  let rec match_depth tona =
    match tona with 
    | LEAF _ -> 1
    | NODE (tona1, tona2) ->
       if member_of tona1 then
         if member_of tona2 then 0
         else (match_depth tona1) + 1
       else
         if member_of tona2 then
           (match_depth tona2) + 1
         else
           raise Error_Input
  in
  let depth = match_depth tona
  in
  let rec find_until tona depth =
    match tona with 
    | LEAF _ -> 1
    | NODE (tona1, tona2) ->
       if depth = 0 then 2
       else
         (find_until tona1 (depth - 1)) + 
           (find_until tona2 (depth - 1)) 
  in
  let result = find_until tona depth 
  in
  print_string
    ("Teams = ("^(string_of_team team1)^", "
     ^(string_of_team team2)^")   \tDepth = "^(string_of_int depth)^"\tAnswer = "^(string_of_int result)^"\n")


let _ = meetin tourna06 (England, Korea)
       ;meetin tourna06 (Sweden, Brazil)
       ;meetin tourna06 (Korea, Brazil)
       ;meetin tourna06 (Korea, Portugal)

	       

let _ = printf "\n===== Excercise. 6 =====\n"

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

let rec calc exp =
  match exp with
  | NUM x         -> x
  | PLUS (x,y)    -> (calc x) + (calc y)
  | MINUS (x,y)   -> (calc x) - (calc y)
				  
let rec eval exp =
  match exp with
  | TRUE          -> true
  | FALSE         -> false
  | NOT x         -> not (eval x)
  | ANDALSO (x,y) -> eval x && eval y
  | ORELSE (x,y)  -> eval x || eval y
  (* | IMPLY (x,y)   -> if ((eval(x)) = (eval(y))) then eval(TRUE) else eval(FALSE) *)
  (* | LESS (x,y)    -> if (calc(x) < calc(y)) then eval(TRUE) else eval(FALSE) *)

  | IMPLY (x,y)   -> not (eval x) || eval y 
  | LESS (x,y)    -> calc x < calc y

								     
let _ = printf "(eval (LESS(PLUS(NUM 3,NUM 5), MINUS(NUM 5,NUM 3)))) : %B\n" (eval (LESS(PLUS(NUM 3,NUM 5), MINUS(NUM 5,NUM 3))))
let _ = printf "(eval (LESS(PLUS(NUM 1,NUM 2), MINUS(NUM 9,NUM 1)))) : %B\n" (eval (LESS(PLUS(NUM 1,NUM 2), MINUS(NUM 9,NUM 1))))



let _ = printf "\n===== Excercise. 7 =====\n"


type nat = ZERO | SUCC of nat

let rec toString x =
  match x with
  | ZERO -> "ZERO"
  | SUCC x -> "SUCC("^(toString x)^")"

let rec natadd x y = 
  match x with
  | ZERO -> y
  | SUCC x -> SUCC (natadd x y) 

let rec natmul x y =
  match x with
  | ZERO -> ZERO
  | SUCC ZERO -> y
  | SUCC x -> (natadd y (natmul x y))
                 
let x = natadd (SUCC(ZERO)) (SUCC(SUCC(SUCC(ZERO))))
let _ = printf "(natadd (SUCC(ZERO)) (SUCC(SUCC(SUCC(ZERO))))) : %s\n" (toString x)
let x = natmul (SUCC(ZERO)) (SUCC(SUCC(SUCC(ZERO))))
let _ = printf "(natmul (SUCC(ZERO)) (SUCC(SUCC(SUCC(ZERO))))) : %s\n" (toString x)
let x = natmul (SUCC(SUCC(ZERO))) (SUCC(SUCC(SUCC(ZERO))))
let _ = printf "(natmul (SUCC(SUCC(ZERO))) (SUCC(SUCC(SUCC(ZERO))))) : %s\n" (toString x)
let x = natmul ZERO (SUCC(SUCC(SUCC(ZERO))))
let _ = printf "(natmul ZERO (SUCC(SUCC(SUCC(ZERO))))) : %s\n" (toString x)
let x = natmul (SUCC(SUCC(SUCC(ZERO)))) ZERO 
let _ = printf "(natmul (SUCC(SUCC(SUCC(ZERO)))) ZERO ) : %s\n" (toString x)
               
let rec nat2n x =
  match x with 
  | ZERO -> 0
  | SUCC x -> 1 + (nat2n x)

let x = nat2n (SUCC(SUCC(SUCC(ZERO))))
let _ = printf "(nat2n (SUCC(SUCC(SUCC(ZERO))))) : %d\n" x
	       
let rec n2nat x =
  match x with
  | 0 -> ZERO
  | _ -> SUCC (n2nat (x - 1))

let x = n2nat 3
let _ = printf "(n2nat 3) : %s\n" (toString x)



let _ = printf "\n===== Excercise. 8 =====\n"

type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val v =
  let k = 2 in 
  let rec kn n = 
    match n with
    | 0 -> 1
    | _ -> k * (kn (n - 1)) in
  let rec calc x n =
    match x with
    | NIL         -> 0
    | ZERO x      -> calc x (n + 1)
    | ONE x       -> 1 * (kn n) + (calc x (n + 1))
    | MONE x      -> -1 * (kn n) + (calc x (n + 1))
  in calc v 0 
	  
let _ = printf "(crazy2val (ZERO(ONE(MONE NIL)))) : %d\n" (crazy2val (ZERO(ONE(MONE NIL))))
let _ = printf "(crazy2val (ONE(ZERO(ONE NIL)))) : %d\n" (crazy2val (ONE(ZERO(ONE NIL))))
let _ = printf "(crazy2val (ONE(MONE(ZERO(MONE NIL))))) : %d\n" (crazy2val (ONE(MONE(ZERO(MONE NIL)))))



let _ = printf "\n===== Excercise. 9 =====\n"
		       
let rec crazy2add (x ,y) =
  match x, y with
  | NIL, x
  | x, NIL             -> x
  | ZERO x, ZERO y     
  | ONE x, MONE y      
  | MONE x, ONE y      -> ZERO (crazy2add (x, y))
  | ZERO x, ONE y      
  | ONE x, ZERO y      -> ONE (crazy2add (x, y))
  | ZERO x, MONE y     
  | MONE x, ZERO y     -> MONE (crazy2add (x, y))

  | ONE x, ONE y       ->
     let carry_one  = crazy2add (ONE NIL ,x) 
     in ZERO (crazy2add (carry_one, y))

  | MONE x, MONE y     ->
     let carry_mone = crazy2add (MONE NIL, x)
     in ZERO (crazy2add (carry_mone, y))			       

let _ = printf "x : %d\n" (crazy2val (ZERO(ONE(MONE NIL))))
let _ = printf "y : %d\n" (crazy2val (ONE(ZERO(ONE NIL))))
let _ = printf "x + y : %d\n" (crazy2val (crazy2add((ZERO(ONE(MONE NIL))),(ONE(ZERO(ONE NIL))))))

let _ = printf "x : %d\n" (crazy2val (ONE(ZERO(ONE NIL))))
let _ = printf "y : %d\n" (crazy2val (ONE(MONE(ZERO(MONE NIL)))))
let _ = printf "x + y : %d\n" (crazy2val (crazy2add((ONE(ZERO(ONE NIL))),(ONE(MONE(ZERO(MONE NIL)))))))

let num = ONE(ONE(ONE(ONE(ONE(ONE NIL)))))
let num2 = ZERO(ZERO(ZERO(ZERO(ZERO(ONE NIL)))))
	     
let _ = printf "x : %d\n" (crazy2val (ONE(ONE(ONE(ONE(ONE(ONE NIL)))))))
let _ = printf "y : %d\n" (crazy2val num2)
let _ = printf "x + y : %d\n" (crazy2val (crazy2add (num, num2)))

