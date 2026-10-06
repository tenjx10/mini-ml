include Utils

let parse (s : string) : prog option =
  match Parser.parse s with
  | prog -> Some prog
  | exception _ -> None

type solution = (string * ty) list

let rec free_var = function
  | TUnit | TInt | TFloat | TBool -> VarSet.empty
  | TVar x -> VarSet.singleton x
  | TList t1 | TOption t1 -> free_var t1
  | TPair (t1, t2) | TFun (t1, t2) -> VarSet.union (free_var t1) (free_var t2)

let ty_subst (ty : ty) (x : string) : ty -> ty =
  let rec go = function
    | TUnit -> TUnit
    | TInt -> TInt
    | TFloat -> TFloat
    | TBool -> TBool
    | TList t -> TList (go t)
    | TOption t -> TOption (go t)
    | TPair (t1, t2) -> TPair (go t1, go t2)
    | TFun (t1, t2) -> TFun (go t1, go t2)
    | TVar y -> 
      if x = y 
      then ty 
      else TVar y
  in go

let rec unify (s : solution) (cs : constr list) : solution option =
  let rec go = function
    | [] -> Some (List.rev s)
    | (t1, t2) :: cs when t1 = t2 -> go cs
    | (TFun (s1, s2), TFun (t1, t2)) :: cs | (TPair (s1, s2), TPair (t1, t2)) :: cs -> go ((s1, t1) :: (s2, t2) :: cs)
    | (TList s1, TList t1) :: cs | (TOption s1, TOption t1) :: cs -> go ((s1, t1) :: cs)
    | (TVar x, ty) :: cs when not (VarSet.mem x (free_var ty)) -> unify ((x, ty) :: s) (List.map (fun (t1, t2) -> (ty_subst ty x t1, ty_subst ty x t2)) cs)
    | (ty, TVar x) :: cs -> go ((TVar x, ty) :: cs)
    | _ -> None
  in go cs
let unify = unify []

let principle_type (ty : ty) (cs : constr list) : ty_scheme option = 
  match unify cs with
  | None -> None
  | Some subst ->
      let t1 = List.fold_left (fun acc (x, t) -> ty_subst t x acc) ty subst in
      Some (Forall ((free_var t1), t1))

let instantiate (Forall (bvs, ty) : ty_scheme) : ty =
  VarSet.fold
    (fun v acc -> ty_subst (TVar (gensym ())) v acc)
    bvs
    ty

let env_add x ty = Env.add x (Forall (VarSet.empty, ty))

let type_of (ctxt: stc_env) (e : expr) : ty_scheme option = 
  let rec go ctxt e =
    match e with 
    | Unit -> TUnit, []
    | Bool _ -> TBool, []
    | Nil -> 
        let fresh = gensym () in
        TList (TVar fresh), []

    | ENone ->
        let fresh = gensym () in
        TOption (TVar fresh), []

    | Int _ -> TInt, []
    | Float _ -> TFloat, []
    | Var x -> (
      match Env.find_opt x ctxt with
      | Some ty -> instantiate ty, []
      | None -> TInt, [TInt, TBool])

    | Assert e ->
        let ty, cs = go ctxt e in
        if e = Bool false 
        then
          let fresh = gensym () in
          TVar fresh, []
        else
          TUnit, (ty, TBool) :: cs

    | ESome e ->
        let ty, cs = go ctxt e in
        TOption ty, cs

    | Bop (op, e1, e2) -> 
        let t1, cs1 = go ctxt e1 in
        let t2, cs2 = go ctxt e2 in
        (match op with
         | Add | Sub | Mul | Div | Mod -> 
            TInt, (t1, TInt) :: (t2, TInt) :: cs1 @ cs2
         | AddF | SubF | MulF | DivF | PowF -> 
            TFloat, (t1, TFloat) :: (t2, TFloat) :: cs1 @ cs2
         | Lt | Lte | Gt | Gte | Eq | Neq -> 
            TBool, (t1, t2) :: cs1 @ cs2
         | And | Or -> 
            TBool, (t1, TBool) :: (t2, TBool) :: cs1 @ cs2
         | Comma -> 
            TPair (t1, t2), cs1 @ cs2
         | Cons -> 
            TList t1, (t2, TList t1) :: cs1 @ cs2)
    
    | If (e1, e2, e3) ->
        let t1, cs1 = go ctxt e1 in
        let t2, cs2 = go ctxt e2 in
        let t3, cs3 = go ctxt e3 in
        t3, (t1, TBool) :: (t2, t3) :: cs1 @ cs2 @ cs3

    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } -> 
        let t, cs = go ctxt matched in
        let alpha = TVar (gensym ()) in
        let ctxt' = env_add hd_name alpha ctxt in
        let ctxt'' = env_add tl_name (TList alpha) ctxt' in
        let t1, cs1 = go ctxt'' cons_case in
        let t2, cs2 = go ctxt nil_case in
        t2, (t, TList alpha) :: (t1, t2) :: cs @ cs1 @ cs2

    | OptMatch { matched; some_name; some_case; none_case } ->
        let t, cs = go ctxt matched in
        let alpha = TVar (gensym ()) in
        let ctxt' = env_add some_name alpha ctxt in
        let t1, cs1 = go ctxt' some_case in
        let t2, cs2 = go ctxt none_case in
        t2, (t, TOption alpha) :: (t1, t2) :: cs @ cs1 @ cs2

    | PairMatch { matched; fst_name; snd_name; case } -> 
        let t, cs = go ctxt matched in
        let alpha = TVar (gensym ()) in
        let beta = TVar (gensym ()) in
        let ctxt' = env_add fst_name alpha ctxt in
        let ctxt'' = env_add snd_name beta ctxt' in
        let t', cs' = go ctxt'' case in
        t', (t, TPair (alpha, beta)) :: cs @ cs'

    | Fun (x, t, e) -> (
        match t with
        | Some ty ->
          let ctxt' = env_add x ty ctxt in
          let t1, cs = go ctxt' e in
          TFun (ty, t1), cs
        | None -> 
          let alpha = TVar (gensym ()) in
          let ctxt' = env_add x alpha ctxt in 
          let t1, cs = go ctxt' e in
          TFun (alpha, t1), cs)

    | App (e1, e2) ->
      let t1, c1 = go ctxt e1 in
      let t2, c2 = go ctxt e2 in
      let alpha = gensym () in
      TVar alpha, (t1, TFun (t2, TVar alpha)) :: c1 @ c2

    | Annot (e, t) ->
        let t2, cs = go ctxt e in
        t, (t2, t) :: cs
    
    | Let { is_rec; name; binding; body } -> 
        if is_rec
        then 
          let alpha = TVar (gensym ()) in
          let ctxt' = env_add name alpha ctxt in
          let t1, cs1 = go ctxt' binding in
          let t2, cs2 = go ctxt' body in
          t2, (t1, alpha) :: cs1 @ cs2
        else
          let t1, cs1 = go ctxt binding in
          let t2, cs2 = go (env_add name t1 ctxt) body in
          t2, cs1 @ cs2
  in 
  let ty, cs = go ctxt e in
  principle_type ty cs

let is_well_typed (p : prog) : bool = 
  let rec go ctxt p =
    match p with
    | [] -> true
    | { is_rec; name; binding } :: t -> 
        let e = 
          if is_rec
          then Let { is_rec; name; binding; body= Var(name) }
          else binding 
        in
        (match type_of ctxt e with
         | Some schem -> go (Env.add name schem ctxt) t
         | None -> false)
   in 
   go Env.empty p

exception AssertFail
exception DivByZero
exception CompareFunVals

let eval_expr (env : dyn_env) (e : expr) : value = 
  let rec go env e =
    match e with
    | Unit -> VUnit
    | Bool b -> VBool b
    | Nil -> VList []
    | ENone -> VNone
    | Int n -> VInt n
    | Float f -> VFloat f
    | Var x -> (
        match Env.find_opt x env with
        | Some v -> v
        | None -> assert false)
    | Assert e -> (
        match go env e with
        | VBool true -> VUnit
        | VBool false -> raise AssertFail
        | _ -> assert false)
    | ESome e -> VSome(go env e)

    | Bop (op, e1, e2) -> (
        let v1 = go env e1 in
        let v2 = go env e2 in
        match op with
        | Add -> (
            match v1, v2 with
            | VInt x, VInt y -> VInt (x + y)
            | _ -> assert false)

        | Sub -> (
            match v1, v2 with
            | VInt x, VInt y -> VInt (x - y)
            | _ -> assert false)

        | Mul -> (
            match v1, v2 with
            | VInt x, VInt y -> VInt (x * y)
            | _ -> assert false)

        | Div -> (
            match v1, v2 with
            | VInt _, VInt 0 -> raise DivByZero
            | VInt x, VInt y -> VInt (x / y)
            | _ -> assert false)

        | Mod -> (
             match v1, v2 with
            | VInt _, VInt 0 -> raise DivByZero
            | VInt x, VInt y -> VInt (x mod y)
            | _ -> assert false)

        | AddF -> (
            match v1, v2 with
            | VFloat x, VFloat y -> VFloat (x +. y)
            | _ -> assert false)

        | SubF -> (
            match v1, v2 with
            | VFloat x, VFloat y -> VFloat (x -. y)
            | _ -> assert false)

        | MulF -> (
            match v1, v2 with
            | VFloat x, VFloat y -> VFloat (x *. y)
            | _ -> assert false)

        | DivF -> (
            match v1, v2 with
            | VFloat x, VFloat y -> VFloat (x /. y)
            | _ -> assert false)

        | PowF -> (
            match v1, v2 with
            | VFloat x, VFloat y -> VFloat (x ** y)
            | _ -> assert false)

        | Lt -> (
            match v1, v2 with
            | VClos _, _ | _, VClos _ -> raise CompareFunVals
            | _ -> VBool (v1 < v2))

        | Lte -> (
            match v1, v2 with
            | VClos _, _ | _, VClos _ -> raise CompareFunVals
            | _ -> VBool (v1 <= v2))

        | Gt -> (
            match v1, v2 with
            | VClos _, _ | _, VClos _ -> raise CompareFunVals
            | _ -> VBool (v1 > v2))

        | Gte -> (
            match v1, v2 with
            | VClos _, _ | _, VClos _ -> raise CompareFunVals
            | _ -> VBool (v1 >= v2))

        | Eq -> (
            match v1, v2 with
            | VClos _, _ | _, VClos _ -> raise CompareFunVals
            | _ -> VBool (v1 = v2))

        | Neq -> (
            match v1, v2 with
            | VClos _, _ | _, VClos _ -> raise CompareFunVals
            | _ -> VBool (v1 <> v2))

        | And -> (
            match v1 with
            | VBool false -> VBool false
            | VBool true -> v2
            | _ -> assert false)

        | Or -> (
            match v1 with
            | VBool true -> VBool true
            | VBool false -> v2
            | _ -> assert false)

        | Comma -> VPair (v1, v2) 

        | Cons -> (
            match v2 with
            | VList v2 -> VList (v1 :: v2)
            | _ -> assert false))

    | If (e1, e2, e3) -> (
        match go env e1 with
        | VBool true -> go env e2
        | VBool false -> go env e3
        | _ -> assert false)

    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } -> ( 
        match go env matched with
        | VList (h :: t) -> 
            let env' = Env.add hd_name h env in
            let env'' = Env.add tl_name (VList t) env' in
            go env'' cons_case
        | VList [] -> go env nil_case 
        | _ -> assert false)

    | OptMatch { matched; some_name; some_case; none_case } -> (
        match go env matched with
        | VSome v -> go (Env.add some_name v env) some_case
        | VNone -> go env none_case
        | _ -> assert false)

    | PairMatch { matched; fst_name; snd_name; case } -> (
        match go env matched with
        | VPair (v1, v2) -> 
            let env' = Env.add fst_name v1 env in
            let env'' = Env.add snd_name v2 env' in
            go env'' case
        | _ -> assert false)

    | Fun (x, _, body) -> VClos ({ name = None; arg = x; body = body; env = env })
    | App (e1, e2) -> (
        match go env e1 with
        | VClos { arg = x; body = b; env = clos; name = None } -> 
            let env' = go env e2 in
            go (Env.add x env' clos) b 
        | VClos { arg = x; body = b; env = clos; name = Some n } ->
            let env' = go env e1 in
	          let env'' = Env.add n env' clos in 
	          go (Env.add x (go env e2) env'') b
        | _ -> assert false)

    | Annot (e, _) -> go env e
    | Let { is_rec = b; name = n; binding = bin; body = bod} -> 
        if b
        then 
          match bin with
          | Fun (x, _, body') -> 
              let clos = VClos { name = Some n; arg = x; body = body'; env } in
              let env' = Env.add n clos env in
              go env' bod
          | _ -> assert false
        else
          let v = go env bin in
          let env' = Env.add n v env in
          go env' bod
  in
  go env e

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;binding}] -> Let {is_rec;name;binding;body = Var name}
    | {is_rec;name;binding} :: ls -> Let {is_rec;name;binding;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog ->
    if is_well_typed prog
    then Ok (eval prog)
    else Error TypeError
  | None -> Error ParseError
