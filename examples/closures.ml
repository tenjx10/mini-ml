(* Closures, currying, and let-polymorphism. *)

let compose f g = fun x -> f (g x)
let twice f = compose f f
let add n = fun x -> x + n

let counter_from start =
  let step = 1 in
  fun n -> start + n * step

(* Top-level definitions are generalized, so id can be used at two
   different types in the same expression. *)
let id x = x
let pair_of_ids = (id 1, id true)

let _ = assert (twice (add 3) 10 = 16)
let _ = assert (compose (fun b -> if b then 1 else 0) (fun x -> x > 0) 5 = 1)
let _ = assert (counter_from 100 5 = 105)
let _ = assert (pair_of_ids = (1, true))
