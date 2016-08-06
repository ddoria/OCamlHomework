(* Exercise 5. enQ, deQ *)
(* #use "Ex5.ml";; *)

module type Queue =
sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
end

module IntListQ =
struct
    type element = int list
    type queue = element * element
    exception EMPTY_Q
    let emptyQ = (([]: element), ([]: element))
    let enQ ((q: queue), (e: element)) =
        match q with
        | ((l: element), (r: element)) -> (((List.append e l): element), r)
    let deQ (q: queue) =
        match q with
        | ((l: element), ([]: element)) ->
            let hd = ([List.hd (List.rev l)]: element)
            in let rv = ((List.tl (List.rev l)): element)
            in (hd, (([], rv): queue))
        | (([]: element), (r: element)) ->
            let hd = ([List.hd r]: element)
            in let rv = ((List.tl r): element)
            in (hd, (([], rv): queue))
        | ((l: element), (r: element)) ->
            let hd = ([List.hd r]: element)
            in let rv = ((List.tl r): element)
            in (hd, ((l, rv): queue))
end

let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x, restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2])