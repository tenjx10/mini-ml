(* Hand-written recursive-descent parser. Binary operators are handled by
   precedence climbing; everything else is one function per grammar rule.

   Operator precedence, loosest to tightest:
     ,                    non-associative
     ||                   right
     &&                   right
     < <= > >= = <>       left
     ::                   right
     + - +. -.            left
     * / mod *. /.        left
     **                   left
     assert e, Some e     prefix (argument is an atom)
     f x y                application *)

open Utils
open Lexer

exception Error of string * pos

type state = { toks : (token * pos) array; mutable cur : int }

let peek st = fst st.toks.(st.cur)
let peek2 st = fst st.toks.(min (st.cur + 1) (Array.length st.toks - 1))
let advance st = if peek st <> EOF then st.cur <- st.cur + 1

let fail st msg = raise (Error (msg, snd st.toks.(st.cur)))

let expect st tok =
  if peek st = tok then advance st
  else fail st (Printf.sprintf "expected %s but found %s"
                  (describe tok) (describe (peek st)))

let ident st =
  match peek st with
  | IDENT x -> advance st; x
  | t -> fail st ("expected an identifier but found " ^ describe t)

(* ---------- types ---------- *)

(* ty ::= prod_ty [-> ty]          (-> is right-associative) *)
let rec ty st =
  let t = prod_ty st in
  if peek st = ARROW then (advance st; TFun (t, ty st)) else t

(* prod_ty ::= postfix_ty { * postfix_ty } *)
and prod_ty st =
  let rec loop t =
    if peek st = STAR then (advance st; loop (TPair (t, postfix_ty st))) else t
  in loop (postfix_ty st)

(* postfix_ty ::= atom_ty { list | option } *)
and postfix_ty st =
  let rec loop t =
    match peek st with
    | KW_LIST -> advance st; loop (TList t)
    | KW_OPTION -> advance st; loop (TOption t)
    | _ -> t
  in loop (atom_ty st)

and atom_ty st =
  match peek st with
  | KW_UNIT -> advance st; TUnit
  | KW_INT -> advance st; TInt
  | KW_FLOAT -> advance st; TFloat
  | KW_BOOL -> advance st; TBool
  | TYVAR a -> advance st; TVar a
  | LPAREN -> advance st; let t = ty st in expect st RPAREN; t
  | t -> fail st ("expected a type but found " ^ describe t)

(* ---------- function parameters ---------- *)

(* param ::= x | (x : ty) *)
let param st =
  match peek st with
  | IDENT x -> advance st; (x, None)
  | LPAREN ->
    advance st;
    let x = ident st in
    expect st COLON;
    let t = ty st in
    expect st RPAREN;
    (x, Some t)
  | t -> fail st ("expected a parameter but found " ^ describe t)

let rec params st =
  match peek st with
  | IDENT _ | LPAREN -> let p = param st in p :: params st
  | _ -> []

(* Turn [f (x : a) y : r = body] into [fun (x : a) -> fun y -> (body : r)]. *)
let curry params ret_ty body =
  let body = match ret_ty with Some t -> Annot (body, t) | None -> body in
  List.fold_right (fun (x, t) acc -> Fun (x, t, acc)) params body

(* ---------- expressions ---------- *)

type assoc = Left | Right | NonAssoc

let binop = function
  | COMMA -> Some (Comma, 1, NonAssoc)
  | OR_OR -> Some (Or, 2, Right)
  | AND_AND -> Some (And, 3, Right)
  | LESS -> Some (Lt, 4, Left)
  | LESS_EQ -> Some (Lte, 4, Left)
  | GREATER -> Some (Gt, 4, Left)
  | GREATER_EQ -> Some (Gte, 4, Left)
  | EQUAL -> Some (Eq, 4, Left)
  | NOT_EQUAL -> Some (Neq, 4, Left)
  | CONS -> Some (Cons, 5, Right)
  | PLUS -> Some (Add, 6, Left)
  | MINUS -> Some (Sub, 6, Left)
  | PLUS_DOT -> Some (AddF, 6, Left)
  | MINUS_DOT -> Some (SubF, 6, Left)
  | STAR -> Some (Mul, 7, Left)
  | SLASH -> Some (Div, 7, Left)
  | MOD -> Some (Mod, 7, Left)
  | STAR_DOT -> Some (MulF, 7, Left)
  | SLASH_DOT -> Some (DivF, 7, Left)
  | STAR_STAR -> Some (PowF, 8, Left)
  | _ -> None

let starts_atom = function
  | INT _ | FLOAT _ | IDENT _ | TRUE | FALSE | NONE | LPAREN | LBRACKET -> true
  | _ -> false

(* expr ::= let ... in expr | fun ... -> expr | if ... | match ... | binary *)
let rec expr st =
  match peek st with
  | LET -> let_expr st
  | FUN ->
    advance st;
    let ps = params st in
    if ps = [] then fail st "expected at least one parameter after 'fun'";
    expect st ARROW;
    curry ps None (expr st)
  | IF ->
    advance st;
    let c = expr st in
    expect st THEN;
    let t = expr st in
    expect st ELSE;
    If (c, t, expr st)
  | MATCH -> match_expr st
  | _ -> binary st 0

(* Shared by local [let ... in] and top-level [let]. *)
and binding st =
  expect st LET;
  let is_rec = peek st = REC in
  if is_rec then advance st;
  let name = ident st in
  let ps = params st in
  let ret_ty = if peek st = COLON then (advance st; Some (ty st)) else None in
  expect st EQUAL;
  (is_rec, name, curry ps ret_ty (expr st))

and let_expr st =
  let is_rec, name, binding_ = binding st in
  expect st IN;
  Let { is_rec; name; binding = binding_; body = expr st }

(* Three shapes are supported, with the cases in either order:
     match e with | x, y -> e
     match e with | h :: t -> e | [] -> e
     match e with | Some x -> e | None -> e *)
and match_expr st =
  expect st MATCH;
  let matched = expr st in
  expect st WITH;
  if peek st = BAR then advance st;
  let arm () = expect st ARROW; expr st in
  let next_case () = expect st BAR in
  match peek st, peek2 st with
  | IDENT _, COMMA ->
    let fst_name = ident st in
    expect st COMMA;
    let snd_name = ident st in
    PairMatch { matched; fst_name; snd_name; case = arm () }
  | IDENT _, CONS ->
    let hd_name, tl_name, cons_case = cons_case st arm in
    next_case ();
    let nil_case = nil_case st arm in
    ListMatch { matched; hd_name; tl_name; cons_case; nil_case }
  | LBRACKET, _ ->
    let nil_case = nil_case st arm in
    next_case ();
    let hd_name, tl_name, cons_case = cons_case st arm in
    ListMatch { matched; hd_name; tl_name; cons_case; nil_case }
  | SOME, _ ->
    let some_name, some_case = some_case st arm in
    next_case ();
    let none_case = none_case st arm in
    OptMatch { matched; some_name; some_case; none_case }
  | NONE, _ ->
    let none_case = none_case st arm in
    next_case ();
    let some_name, some_case = some_case st arm in
    OptMatch { matched; some_name; some_case; none_case }
  | t, _ -> fail st ("expected a pattern but found " ^ describe t)

and cons_case st arm =
  let hd = ident st in
  expect st CONS;
  let tl = ident st in
  (hd, tl, arm ())

and nil_case st arm = expect st LBRACKET; expect st RBRACKET; arm ()
and some_case st arm = expect st SOME; let x = ident st in (x, arm ())
and none_case st arm = expect st NONE; arm ()

(* Precedence climbing: parse operators that bind at least as tightly as
   [min_prec]. *)
and binary st min_prec =
  let rec loop lhs =
    match binop (peek st) with
    | Some (op, prec, assoc) when prec >= min_prec ->
      advance st;
      let rhs = binary st (if assoc = Right then prec else prec + 1) in
      (match assoc, binop (peek st) with
       | NonAssoc, Some (_, p, _) when p = prec ->
         fail st ("operator " ^ describe (peek st) ^ " is not associative")
       | _ -> ());
      loop (Bop (op, lhs, rhs))
    | _ -> lhs
  in loop (prefix st)

(* prefix ::= assert atom | Some atom | atom { atom }
   An operand may also be a let/fun/if/match, as in [1 + if b then 1 else 2]. *)
and prefix st =
  match peek st with
  | ASSERT -> advance st; Assert (atom st)
  | SOME -> advance st; ESome (atom st)
  | LET | FUN | IF | MATCH -> expr st
  | _ ->
    let rec apply f =
      if starts_atom (peek st) then apply (App (f, atom st)) else f
    in apply (atom st)

and atom st =
  match peek st with
  | INT n -> advance st; Int n
  | FLOAT f -> advance st; Float f
  | IDENT x -> advance st; Var x
  | TRUE -> advance st; Bool true
  | FALSE -> advance st; Bool false
  | NONE -> advance st; ENone
  | LBRACKET -> advance st; list_items st
  | LPAREN ->
    advance st;
    if peek st = RPAREN then (advance st; Unit)
    else
      let e = expr st in
      let e = if peek st = COLON then (advance st; Annot (e, ty st)) else e in
      expect st RPAREN;
      e
  | t -> fail st ("expected an expression but found " ^ describe t)

(* [e1; e2; e3] becomes e1 :: e2 :: e3 :: [] (a trailing ';' is allowed). *)
and list_items st =
  if peek st = RBRACKET then (advance st; Nil)
  else
    let first = expr st in
    let rec rest () =
      match peek st with
      | SEMI when peek2 st = RBRACKET -> advance st; advance st; []
      | SEMI -> advance st; let e = expr st in e :: rest ()
      | _ -> expect st RBRACKET; []
    in
    List.fold_right (fun e acc -> Bop (Cons, e, acc)) (first :: rest ()) Nil

(* ---------- programs ---------- *)

let prog st =
  let rec go () =
    if peek st = EOF then []
    else
      let is_rec, name, binding_ = binding st in
      { is_rec; name; binding = binding_ } :: go ()
  in go ()

let parse (src : string) : prog =
  prog { toks = Lexer.tokenize src; cur = 0 }
