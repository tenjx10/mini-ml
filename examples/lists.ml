(* Polymorphic higher-order list functions. No annotations are needed:
   every type below is inferred. *)

let rec map f l =
  match l with
  | x :: xs -> f x :: map f xs
  | [] -> []

let rec filter p l =
  match l with
  | x :: xs -> if p x then x :: filter p xs else filter p xs
  | [] -> []

let rec fold_left f acc l =
  match l with
  | x :: xs -> fold_left f (f acc x) xs
  | [] -> acc

let rev l = fold_left (fun acc x -> x :: acc) [] l

let sum l = fold_left (fun a b -> a + b) 0 l

let rec range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi

(* map and fold_left are used at several different types. *)
let _ = assert (map (fun x -> x * x) [1; 2; 3] = [1; 4; 9])
let _ = assert (map (fun x -> x > 1) [1; 2; 3] = [false; true; true])
let _ = assert (filter (fun x -> x mod 2 = 0) (range 1 10) = [2; 4; 6; 8; 10])
let _ = assert (rev [1; 2; 3] = [3; 2; 1])
let _ = assert (rev [true; false] = [false; true])
let _ = assert (sum (range 1 100) = 5050)
