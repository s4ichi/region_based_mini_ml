type ident = string ;;

type exp =
  | IntLit of int
  | Var of ident
  | Let of ident * exp * exp
  | LetReg of ident * exp
  | LetRec of ident * ident * exp * exp
  | Call of exp * exp
  | Lam of ident * exp
  | Empty ;;

module type VARIABLE = sig
  type t
  val intro   : ident -> t
  val equal   : t * t -> bool
  val compare : t * t -> int
end

module TyVar : VARIABLE = struct
  type t = ident
  let intro x = x
  let equal (x, x') = x = x'
  let compare = fun (a,b) -> String.compare a b
  let new_var c = intro ("'t" ^ (string_of_int c))
end

module TyVarSet = Set.Make(TyVar.t) ;;

module RegVar : VARIABLE = struct
  type t = ident
  let intro x = x
  let equal (x, x') = x = x'
  let compare = fun (a,b) -> String.compare a b
  let new_var c = intro ("'r" ^ (string_of_int c))
end

module RegVarSet = Set.Make(RegVar.t) ;;

module EffVar : VARIABLE = struct
  type t = ident
  let intro x = x
  let equal (x, x') = x = x'
  let compare = fun (a,b) -> String.compare a b
  let new_var c = intro ("'e" ^ (string_of_int c))
end

module EffVarSet = Set.Make(EffVar.t) ;;

module VarStream = struct
  let intro = (0, 0, 0)
  let fresh_type_var (a, b, c) =
    (TypeVar.new_var a, (a+1, b, c))
  let fresh_reg_var (a, b, c) =
    (RegVar.new_var b, (a, b+1, c))
  let fresh_eff_var (a, b, c) =
    (EffVar.new_var c, (a, b, c+1))
end

(* atomic_effect * effect var *)
type effect =
  | EffVar of EffVar.t
  | EffPut of RegVar.t
  | EffGet of RegVar.t

module EffSet = Set.Make(effect) ;;

(* effect var * effect *)
module ArrowEff = struct
  type t = EffVar.t * EffSet.t

  let frv (_, phi) =
    RegSet.fold (function
    | (EffPut(r), set) -> RegSet.add r set
    | (EffGet(r), set) -> RegSet.add r set
    | (_, set) -> set
    ) RegSet.empty phi

  let fev (eps, phi) =
    EffSet.union
      (EffSet.singleton eps)
      (
        EffSet.fold (function
        | (EffVar(s), set) -> EffSet.add s set
        | (_, set) -> set
        ) EffSet.empty phi
      )
end

type
  target_ty =
  | TVar of TypeVar.t
  | TInt
  | TBool
  | TArrow of ty_with_place * ArrowEff.t * ty_with_place
and
  ty_with_place = target_ty * RegVar.t (* type * region var *)

module AnnotatedType = struct
  type t = ty_with_place * EffectSet.t

  let fresh_annnotated_ty vs = ;;

  let rec ftv ty = match ty with
    | (TVar(s), _) -> TyVarSet.singleton s
    | (TArrow(t1, (ev, eff), t2), _) -> TyVarSet.union (ftv t1) (ftv t2)
    | (_, _) -> TyVarSet.empty ;;

  let rec frv ty = match ty with
    | (TArrow(t1, (ev, eff), t2), r) ->
       RegVarSet.union
         (RegVarSet.union (frv t1) (frv t2))
         (RegVarSet.union (RegVarSet.singleton r) (ArrowEff.frv eff))
    | (_, r) -> RegVarSet.singleton r ;;

  let rec fev ty = match ty with
    | (TArrow(t1, (ev, eff), t2), _) ->
       EffVarSet.union
         (EffVarSet.union (fev t1) (fev t2))
         (EffSet.union (EffVarSet.singleton ev) (ArrowEff.fev eff))
    | (_, _) -> EffVarSet.empty ;;

  let fv ty = ((ftv ty), (frv ty), (fev ty))

  let unify = failwith 0
end

module TypedExp = struct
  type t = exp * AnnotatedType.t
end

module EffVarMap = Map.Make(EffVar.t)
module RegVarMap = Map.Make(RegVar.t)
module TyVarMap = Map.Make(TyVar.t)

module Substitute = struct
  let compose s1 s2 = failwith 0 ;;

  let subst_reg ((st, sr, se), r) =
    match RegVarMap.find r sr with
    | None -> r
    | Some r' = r'

  let subst_eff ((st, sr, se) as s, eff) =
    EffSet.fold (function
    | (EffPut(r), set) -> EffSet.add (EffPut(subst_reg s r)) set
    | (EffGet(r), set) -> EffSet.add (EffGet(subst_reg s r)) set
    | (EffVar(x), set) -> EffSet.add (
      EffVar(match EffVarMap.find x se with
      | None -> x
      | Some x' -> x')
    ) set) EffSet.empty eff

  let subst_arrow_eff ((st, sr, se) as s, (eps, eff)) =
    let (eps1, eff1) =
      match EffVarMap.find eps se with
      | None -> (eps, eff)
      | Some (eps', eff') -> (eps', eff') in
    (eps1, subst_eff s eff1)

  let rec subst_ty ((st, sr, se) as s, t) =
    match t with
    | TInt -> TInt
    | TBool -> TBool
    | TVar(x) ->
       (match TyVarMap.find x st with
       | None -> t
       | Some x' -> x')
    | TArrow((t1, r1), ae, (t2, r2)) ->
       TArrow(
         ((subst_ty s t1), (subst_reg s r1)),
         subst_arrow_eff s ae,
         ((subst_ty s t2), (subst_reg s r2))
       )

  let rec subst (s, (t, r)) = ((subst_ty s t), (subst_reg s r))
end

module IndentEnv = Map.Make(String) ;;

module type TRANSLATOR = sig
  val translate : exp -> TypedExp.t
end

module TargetTranslator = struct
  (* translate: exp -> TypedExp.t *)
  let translate exp =
    let unify = 1 in
    let rec walk e env =
      match e with
      | IntLit(n) ->
         let rv = RegVar.new_var in
	       (IntLit(n), (int, rv), put(rv))
      | Var(s) ->
         let mu = TypeEnv.find s env in
         (Var(s), mu, [])
      | Call(e1,e2) ->
      | Lam(x,e1) ->
      | Let(x,e1,e2) ->
         let (_,twp1,ef1) = walk e1 env in
         let ex_env = TypeEnv.add x twp1 env in
         let (_,twp2,ef2) = walk e2 ex_env in
         (Let(x, e1, e2), twp2, ef1 @ ef2)
      | LetRec(f,x,e1,e2) -> failwith "unsupported let rec"
      | _ -> failwith "unknown expression"
    in
    walk TypeEnv.empty ;;
end
