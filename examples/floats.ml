(* Floating-point arithmetic: square roots by Newton's method. *)

let abs_float x = if x < 0. then 0. -. x else x

let sqrt x =
  let rec go guess n =
    if n = 0 then guess
    else go ((guess +. x /. guess) /. 2.) (n - 1)
  in go x 20

let close_to a b = abs_float (a -. b) < 0.000001

let _ = assert (close_to (sqrt 2.) 1.414213)
let _ = assert (close_to (sqrt 144.) 12.)
let _ = assert (close_to (2. ** 10.) 1024.)
