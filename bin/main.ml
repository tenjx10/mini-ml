(* Command-line entry point: type-check and run a program file. *)

open Miniml

let usage () =
  prerr_endline "usage: miniml FILE";
  exit 2

let fail fmt = Printf.ksprintf (fun msg -> prerr_endline msg; exit 1) fmt

(* Re-parse to recover the position of a syntax error for the message. *)
let parse_error src =
  match Parser.parse src with
  | exception Lexer.Error (msg, { line; col }) | exception Parser.Error (msg, { line; col }) ->
    fail "syntax error (line %d, column %d): %s" line col msg
  | _ -> fail "syntax error"

let () =
  let file = if Array.length Sys.argv = 2 then Sys.argv.(1) else usage () in
  let src =
    try In_channel.with_open_text file In_channel.input_all
    with Sys_error msg -> fail "error: %s" msg
  in
  match Interp.interp src with
  | Ok _ -> ()
  | Error Utils.ParseError -> parse_error src
  | Error Utils.TypeError -> fail "type error"
  | exception Interp.AssertFail -> fail "runtime error: assertion failed"
  | exception Interp.DivByZero -> fail "runtime error: division by zero"
  | exception Interp.CompareFunVals -> fail "runtime error: cannot compare functions"
