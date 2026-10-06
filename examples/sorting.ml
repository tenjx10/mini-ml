(* Insertion sort and merge sort over any ordered type. *)

let rec insert x l =
  match l with
  | y :: ys -> if x <= y then x :: l else y :: insert x ys
  | [] -> [x]

let rec insertion_sort l =
  match l with
  | x :: xs -> insert x (insertion_sort xs)
  | [] -> []

let rec split l =
  match l with
  | x :: rest ->
    (match rest with
     | y :: ys -> (match split ys with | a, b -> (x :: a, y :: b))
     | [] -> ([x], []))
  | [] -> ([], [])

let rec merge a b =
  match a with
  | x :: xs ->
    (match b with
     | y :: ys -> if x <= y then x :: merge xs b else y :: merge a ys
     | [] -> a)
  | [] -> b

let rec merge_sort l =
  match l with
  | x :: rest ->
    (match rest with
     | [] -> [x]
     | _ :: _ -> (match split l with | a, b -> merge (merge_sort a) (merge_sort b)))
  | [] -> []

let _ = assert (insertion_sort [5; 2; 9; 1; 5; 6] = [1; 2; 5; 5; 6; 9])
let _ = assert (merge_sort [5; 2; 9; 1; 5; 6] = [1; 2; 5; 5; 6; 9])
let _ = assert (merge_sort [3.5; -1.; 2.25] = [-1.; 2.25; 3.5])
let _ = assert (merge_sort [true; false; true] = [false; true; true])
