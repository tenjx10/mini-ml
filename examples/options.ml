(* Options, pairs, and type annotations. *)

let safe_div (a : int) (b : int) : int option =
  if b = 0 then None else Some (a / b)

let with_default d o =
  match o with
  | Some x -> x
  | None -> d

let rec find p l =
  match l with
  | x :: xs -> if p x then Some x else find p xs
  | [] -> None

let rec zip a b =
  match a with
  | x :: xs ->
    (match b with
     | y :: ys -> (x, y) :: zip xs ys
     | [] -> [])
  | [] -> []

let swap p = match p with | a, b -> (b, a)

let _ = assert (safe_div 10 2 = Some 5)
let _ = assert (with_default 0 (safe_div 1 0) = 0)
let _ = assert (find (fun x -> x > 3) [1; 5; 2; 7] = Some 5)
let _ = assert (zip [1; 2; 3] [true; false] = [(1, true); (2, false)])
let _ = assert (swap (1, true) = (true, 1))
