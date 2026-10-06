# mini-ml

An interpreter for a small statically typed functional language with OCaml-like syntax, written in OCaml. Programs are parsed, type-checked with **Hindley–Milner type inference**, and evaluated. You don't have to write any type annotations.

```ocaml
let rec map f l =
  match l with
  | x :: xs -> f x :: map f xs
  | [] -> []

(* map : ('a -> 'b) -> 'a list -> 'b list is inferred, then used at two types *)
let _ = assert (map (fun x -> x * x) [1; 2; 3] = [1; 4; 9])
let _ = assert (map (fun x -> x > 1) [1; 2; 3] = [false; true; true])
```

## Features

- **Types:** `int`, `float`, `bool`, `unit`, lists, options, pairs, functions and type variables (`'a`)
- **Type inference:** constraint generation plus unification with an occurs check; top-level definitions are generalized, so they're polymorphic
- **Optional annotations:** `let f (x : int) : int list = ...`, `(e : t)`
- **Functions:** first-class closures, currying and `let rec`
- **Pattern matching:** on lists (`h :: t` / `[]`), options (`Some x` / `None`) and pairs (`a, b`)
- **Operators:** int `+ - * / mod`, float `+. -. *. /. **`, comparisons `< <= > >= = <>`, `&& ||`, `::`
- **Errors:** syntax errors report the line and column; failed `assert`, division by zero and comparing functions are reported as runtime errors

## How it works

| Stage | File | Description |
|---|---|---|
| Lexer | [`lib/lexer.ml`](lib/lexer.ml) | Hand-written scanner. Handles nested comments, and tells negative literals (`f (-1)`) apart from subtraction (`n-1`). |
| Parser | [`lib/parser.ml`](lib/parser.ml) | Recursive descent with precedence climbing for binary operators; desugars multi-argument functions and list literals. |
| Type checker | [`lib/interp.ml`](lib/interp.ml) | Walks the AST to produce a type and a list of equality constraints, solves them by unification, then generalizes free type variables into a type scheme. |
| Evaluator | [`lib/interp.ml`](lib/interp.ml) | Environment-based big-step evaluation with closures; recursive functions are named closures. |

The AST and type definitions are in [`lib/utils.ml`](lib/utils.ml).

## Building and running

Requires OCaml (≥ 5.0), [dune](https://dune.build), and [OUnit2](https://github.com/gildor478/ounit) for the tests:

```sh
opam install dune ounit2
dune build
dune exec ./bin/main.exe examples/lists.ml   # no output means every assert passed
dune test
```

## Examples

[`examples/`](examples/) contains programs covering recursion, higher-order list functions, insertion sort and merge sort, options and pairs, floating-point math, and closures.

## Limitations/Potential Future Updates

- Local `let` bindings are not generalized (only top-level ones are), so `let id = fun x -> x in (id 1, id true)` is rejected.
- `&&` and `||` evaluate both operands.
- Patterns are one level deep, and there are no strings, records or user-defined types.

## Background

This started as a course project in CS 320 (Concepts of Programming Languages) at Boston University. The language design and AST definitions in `lib/utils.ml` come from the course. The lexer, parser, type inference, evaluator, tests and examples are my own.
