open OUnit2
open Miniml
open Utils

let parses_to src expected _ =
  assert_equal ~printer:(fun _ -> "<ast>") (Some expected) (Interp.parse src)

let parse_fails src _ =
  assert_equal None (Interp.parse src) ~msg:("should not parse: " ^ src)

let runs src _ =
  match Interp.interp src with
  | Ok _ -> ()
  | Error e -> assert_failure (err_msg e ^ " in: " ^ src)

let type_error src _ =
  assert_equal (Error TypeError) (Interp.interp src)
    ~printer:(function Ok _ -> "Ok" | Error e -> err_msg e)

let raises exn src _ =
  assert_raises exn (fun () -> Interp.interp src)

(* Expected AST for [let x = e]. *)
let def e = [ { is_rec = false; name = "x"; binding = e } ]
let num n = Int n
let ( +: ) a b = Bop (Add, a, b)
let ( *: ) a b = Bop (Mul, a, b)


let parser_tests =
  "parser" >::: [
    "precedence" >:: parses_to "let x = 1 + 2 * 3"
      (def (num 1 +: (num 2 *: num 3)));
    "left assoc" >:: parses_to "let x = 1 - 2 - 3"
      (def (Bop (Sub, Bop (Sub, num 1, num 2), num 3)));
    "cons is right assoc" >:: parses_to "let x = 1 :: 2 :: []"
      (def (Bop (Cons, num 1, Bop (Cons, num 2, Nil))));
    "list literal" >:: parses_to "let x = [1; 2;]"
      (def (Bop (Cons, num 1, Bop (Cons, num 2, Nil))));
    "application binds tightest" >:: parses_to "let x = f 1 + g 2"
      (def (App (Var "f", num 1) +: App (Var "g", num 2)));
    "parens" >:: parses_to "let x = (1 + 2) * 3"
      (def ((num 1 +: num 2) *: num 3));
    "negative literal" >:: parses_to "let x = f (-1)"
      (def (App (Var "f", num (-1))));
    "binary minus without spaces" >:: parses_to "let x = n-1"
      (def (Bop (Sub, Var "n", num 1)));
    "curried function with annotations" >::
      parses_to "let f (x : int) y : bool = y"
        [ { is_rec = false; name = "f";
            binding = Fun ("x", Some TInt, Fun ("y", None, Annot (Var "y", TBool))) } ];
    "arrow is right assoc, list binds tightest" >::
      parses_to "let x = (f : int -> int list -> bool)"
        (def (Annot (Var "f", TFun (TInt, TFun (TList TInt, TBool)))));
    "match cases in either order" >::
      parses_to "let x = match l with | [] -> 0 | h :: t -> h"
        (def (ListMatch { matched = Var "l"; hd_name = "h"; tl_name = "t";
                          cons_case = Var "h"; nil_case = num 0 }));
    "nested comments" >:: parses_to "let x = (* a (* b *) c *) 1" (def (num 1));
    "comma is non-associative" >:: parse_fails "let x = 1, 2, 3";
    "missing in" >:: parse_fails "let x = let y = 1 y";
    "unterminated comment" >:: parse_fails "let x = 1 (* oops";
    "unknown constructor" >:: parse_fails "let x = Foo";
  ]


let typing_tests =
  "typing" >::: [
    "if branches must agree" >:: type_error "let x = if true then 1 else false";
    "condition must be bool" >:: type_error "let x = if 1 then 2 else 3";
    "int ops reject floats" >:: type_error "let x = 1 + 2.0";
    "float ops reject ints" >:: type_error "let x = 1 +. 2.";
    "unbound variable" >:: type_error "let x = y";
    "occurs check" >:: type_error "let f x = x x";
    "annotation mismatch" >:: type_error "let x : bool = 5";
    "lists are homogeneous" >:: type_error "let x = [1; true]";
    "top-level let is polymorphic" >:: runs "let id x = x let p = (id 1, id true)";
    "assert false has any type" >:: runs "let f x = if x then 1 else assert false";
    "inference through recursion" >::
      runs "let rec len l = match l with | _ :: t -> 1 + len t | [] -> 0 \
            let _ = assert (len [true; false] = 2)";
  ]


let eval_tests =
  "eval" >::: [
    "arithmetic" >:: runs "let _ = assert (7 / 2 = 3 && 7 mod 2 = 1)";
    "closures capture env" >::
      runs "let add n = fun x -> x + n let _ = assert (add 2 3 = 5)";
    "structural equality" >:: runs "let _ = assert ([(1, Some true)] = [(1, Some true)])";
    "ordering" >:: runs "let _ = assert ([1; 2] < [1; 3] && None < Some 0)";
    "failed assert" >:: raises Interp.AssertFail "let _ = assert (1 = 2)";
    "division by zero" >:: raises Interp.DivByZero "let x = 1 / 0";
    "mod by zero" >:: raises Interp.DivByZero "let x = 1 mod 0";
    "comparing functions" >::
      raises Interp.CompareFunVals "let f x = x let _ = f = f";
  ]

(* every program in examples/ must run without errors. *)
let example_tests =
  let dir = "../examples" in
  "examples" >:::
    (Sys.readdir dir |> Array.to_list |> List.sort compare
     |> List.filter (fun f -> Filename.check_suffix f ".ml")
     |> List.map (fun f ->
         f >:: runs (In_channel.with_open_text (Filename.concat dir f)
                       In_channel.input_all)))

let () =
  run_test_tt_main
    ("miniml" >::: [ parser_tests; typing_tests; eval_tests; example_tests ])
