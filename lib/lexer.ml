(* lexer.ml *)

type token =
  (* literals and names *)
  | INT of int
  | FLOAT of float
  | IDENT of string
  | TYVAR of string
  (* keywords *)
  | LET | REC | IN | FUN | IF | THEN | ELSE | MATCH | WITH
  | TRUE | FALSE | ASSERT | SOME | NONE | MOD
  | KW_UNIT | KW_INT | KW_FLOAT | KW_BOOL | KW_LIST | KW_OPTION
  (* punctuation *)
  | LPAREN | RPAREN | LBRACKET | RBRACKET
  | COMMA | SEMI | COLON | BAR | ARROW
  (* operators *)
  | PLUS | MINUS | STAR | SLASH
  | PLUS_DOT | MINUS_DOT | STAR_DOT | SLASH_DOT | STAR_STAR
  | CONS | EQUAL | NOT_EQUAL | LESS | LESS_EQ | GREATER | GREATER_EQ
  | AND_AND | OR_OR
  | EOF

type pos = { line : int; col : int }

exception Error of string * pos

let keywords =
  [ "let", LET; "rec", REC; "in", IN; "fun", FUN
  ; "if", IF; "then", THEN; "else", ELSE
  ; "match", MATCH; "with", WITH
  ; "true", TRUE; "false", FALSE; "assert", ASSERT; "mod", MOD
  ; "unit", KW_UNIT; "int", KW_INT; "float", KW_FLOAT; "bool", KW_BOOL
  ; "list", KW_LIST; "option", KW_OPTION
  ]

(* longest symbols first, so that e.g. "->" wins over "-". *)
let symbols =
  [ "->", ARROW; "::", CONS; "<=", LESS_EQ; ">=", GREATER_EQ; "<>", NOT_EQUAL
  ; "&&", AND_AND; "||", OR_OR; "**", STAR_STAR
  ; "+.", PLUS_DOT; "-.", MINUS_DOT; "*.", STAR_DOT; "/.", SLASH_DOT
  ; "(", LPAREN; ")", RPAREN; "[", LBRACKET; "]", RBRACKET
  ; ",", COMMA; ";", SEMI; ":", COLON; "|", BAR; "=", EQUAL
  ; "+", PLUS; "-", MINUS; "*", STAR; "/", SLASH; "<", LESS; ">", GREATER
  ]

let is_digit c = '0' <= c && c <= '9'
let is_lower c = ('a' <= c && c <= 'z') || c = '_'
let is_upper c = 'A' <= c && c <= 'Z'
let is_ident_char c = is_lower c || is_upper c || is_digit c || c = '\''

(* A '-' directly before a digit is a negative literal only when it cannot be
   a binary minus, i.e. when the previous token does not end an operand.
   So [n-1] is subtraction while [f (-1)] and [x = -1] are literals. *)
let ends_operand = function
  | INT _ | FLOAT _ | IDENT _ | RPAREN | RBRACKET | TRUE | FALSE | NONE -> true
  | _ -> false

let tokenize (src : string) : (token * pos) array =
  let len = String.length src in
  let i = ref 0 and line = ref 1 and line_start = ref 0 in
  let toks = ref [] in
  let pos_at k = { line = !line; col = k - !line_start + 1 } in
  let peek k = if !i + k < len then Some src.[!i + k] else None in
  let starts_with s =
    let n = String.length s in
    !i + n <= len && String.sub src !i n = s
  in
  let take_while p =
    let start = !i in
    while !i < len && p src.[!i] do incr i done;
    String.sub src start (!i - start)
  in
  let newline () = incr line; line_start := !i + 1 in
  let rec skip_comment depth start_pos =
    if !i >= len then raise (Error ("unterminated comment", start_pos))
    else if starts_with "(*" then (i := !i + 2; skip_comment (depth + 1) start_pos)
    else if starts_with "*)" then (
      i := !i + 2;
      if depth > 0 then skip_comment (depth - 1) start_pos)
    else (
      if src.[!i] = '\n' then newline ();
      incr i;
      skip_comment depth start_pos)
  in
  let number start_pos negative =
    let digits = take_while is_digit in
    let sign = if negative then "-" else "" in
    if peek 0 = Some '.' then (
      incr i;
      let frac = take_while is_digit in
      FLOAT (float_of_string (sign ^ digits ^ "." ^ frac)))
    else
      match int_of_string_opt (sign ^ digits) with
      | Some n -> INT n
      | None -> raise (Error ("integer literal out of range", start_pos))
  in
  let prev_ends_operand () =
    match !toks with
    | (t, _) :: _ -> ends_operand t
    | [] -> false
  in
  while !i < len do
    let c = src.[!i] in
    let p = pos_at !i in
    if c = '\n' then (newline (); incr i)
    else if c = ' ' || c = '\t' || c = '\r' then incr i
    else if starts_with "(*" then (i := !i + 2; skip_comment 0 p)
    else
      let tok =
        if is_digit c then number p false
        else if c = '-' && Option.fold ~none:false ~some:is_digit (peek 1)
                && not (prev_ends_operand ())
        then (incr i; number p true)
        else if is_lower c then
          let word = take_while is_ident_char in
          Option.value (List.assoc_opt word keywords) ~default:(IDENT word)
        else if is_upper c then
          match take_while is_ident_char with
          | "Some" -> SOME
          | "None" -> NONE
          | word -> raise (Error ("unknown constructor '" ^ word ^ "'", p))
        else if c = '\'' && Option.fold ~none:false ~some:is_lower (peek 1) then (
          incr i;
          TYVAR ("'" ^ take_while is_ident_char))
        else
          match List.find_opt (fun (s, _) -> starts_with s) symbols with
          | Some (s, t) -> i := !i + String.length s; t
          | None -> raise (Error (Printf.sprintf "unexpected character '%c'" c, p))
      in
      toks := (tok, p) :: !toks
  done;
  Array.of_list (List.rev ((EOF, pos_at !i) :: !toks))

let describe = function
  | INT n -> string_of_int n
  | FLOAT f -> string_of_float f
  | IDENT x -> "'" ^ x ^ "'"
  | TYVAR a -> a
  | EOF -> "end of input"
  | t ->
    let all = List.map (fun (s, t) -> (t, s)) (keywords @ symbols) in
    match List.assoc_opt t all with
    | Some s -> "'" ^ s ^ "'"
    | None -> (match t with SOME -> "'Some'" | NONE -> "'None'" | _ -> "token")
