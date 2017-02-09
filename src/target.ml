type ident = string ;;

module type SrcExp = sig
  type t =
    | IntLit of int
    | Var of ident
    | Let of ident * t * t
    | LetRec of ident * ident * t * t
    | Call of t * t
    | Lam of ident * t
    | Empty
end

module SrcExp = struct
  type t =
    | IntLit of int
    | Var of ident
    | Let of ident * t * t
    | LetRec of ident * ident * t * t
    | Call of t * t
    | Lam of ident * t
    | Empty
end

module type VARIABLE = sig
  type t
  val intro   : ident -> t
  val equal   : t * t -> bool
  val compare : t -> t -> int
  val new_var : int -> t
end

module TyVar : VARIABLE = struct
  type t = ident
  let intro x = x
  let equal (x, x') = x = x'
  let compare a = fun b -> String.compare a b
  let new_var c = intro ("'t" ^ (string_of_int c))
end

module TyVarSet = Set.Make(TyVar)

module RegVar : VARIABLE = struct
  type t = ident
  let intro x = x
  let equal (x, x') = x = x'
  let compare a = fun b -> String.compare a b
  let new_var c = intro ("'r" ^ (string_of_int c))
end

module RegVarSet = Set.Make(RegVar) ;;

module EffVar : VARIABLE = struct
  type t = ident
  let intro x = x
  let equal (x, x') = x = x'
  let compare a = fun b -> String.compare a b
  let new_var c = intro ("'e" ^ (string_of_int c))
end

module EffVarSet = Set.Make(EffVar) ;;

type effect =
  | EffVar of EffVar.t
  | EffPut of RegVar.t
  | EffGet of RegVar.t

module Effect = struct
  type t = effect
  let compare a = fun b ->
    match (a, b) with
    | (EffVar(s1), EffVar(s2)) ->
       EffVar.compare s1 s2
    | (EffPut(s1), EffPut(s2)) | (EffGet(s1), EffGet(s2)) ->
       RegVar.compare s1 s2
    | (EffVar(s1), EffPut(s2)) -> -1
    | (EffPut(s1), EffVar(s2)) -> 1
    | (EffVar(s1), EffGet(s2)) -> -1
    | (EffGet(s1), EffVar(s2)) -> 1
    | (EffPut(s1), EffGet(s2)) -> -1
    | (EffGet(s1), EffPut(s2)) -> 1
end

module EffSet = Set.Make(Effect) ;;

module EffVarMap = Map.Make(EffVar)
module RegVarMap = Map.Make(RegVar)
module TyVarMap = Map.Make(TyVar)

module ArrowEff = struct
  type t = EffVar.t * EffSet.t

  let frv (_, phi) =
    EffSet.fold
      ( fun a -> ( fun b ->
                   match a with
                   | EffPut(r) -> RegVarSet.add r b
                   | EffGet(r) -> RegVarSet.add r b
                   | _ -> b
                 )
      ) phi RegVarSet.empty

  let fev (eps, phi) =
    EffVarSet.union
      (EffVarSet.singleton eps)
      (
        EffSet.fold
          ( fun a -> ( fun b ->
                       match a with
                       | EffVar(s) -> EffVarSet.add s b
                       | _ -> b
                     )
          ) phi EffVarSet.empty
      )
end

type annotated_type =
  | TVar of TyVar.t
  | TInt of int
  | TBool of bool
  | TArrow of ty_with_place * ArrowEff.t * ty_with_place
and
  ty_with_place = annotated_type * RegVar.t

module Substitute = struct
  let empty = (TyVarMap.empty, RegVarMap.empty, EffVarMap.empty)

  let subst_reg ((st, sr, se), r) =
    if RegVarMap.mem r sr then RegVarMap.find r sr
    else r

  let subst_eff ((st, sr, se) as s, eff) =
    EffSet.fold
      ( fun a -> ( fun b ->
                   match a with
                   | EffPut(r) -> EffSet.add (EffPut(subst_reg (s, r))) b
                   | EffGet(r) -> EffSet.add (EffGet(subst_reg (s, r))) b
                   | EffVar(x) ->
                      let (eps1, eff1) =
                        if EffVarMap.mem x se then EffVarMap.find x se
                        else (x, EffSet.empty)
                      in
                      EffSet.union (EffSet.add (EffVar(eps1)) eff1) b
                 )
      ) EffSet.empty eff

  let subst_arrow_eff ((st, sr, se) as s, (eps, eff)) =
    let (eps1, eff1) =
      if EffVarMap.mem eps se then EffVarMap.find eps se
      else (eps, eff) in
    (eps1, subst_eff (s, eff1))

  let rec subst_ty ((st, sr, se) as s, t) =
    match t with
    | TInt(n) -> TInt(n)
    | TBool(b) -> TBool(b)
    | TVar(x) ->
       if TyVarMap.mem x st then TyVarMap.find x st
       else t
    | TArrow((t1, r1), ae, (t2, r2)) ->
       TArrow(
         ((subst_ty (s, t1)), (subst_reg (s, r1))),
         subst_arrow_eff (s, ae),
         ((subst_ty (s, t2)), (subst_reg (s, r2)))
       )

  let subst s (t, r) = ((subst_ty (s, t)), (subst_reg (s, r)))

  let compose ((st1, sr1, se1) as s1) ((st2, sr2, se2) as s2) =
    (
      TyVarMap.fold (fun k -> fun v -> fun b -> TyVarMap.add k v b) st1 st2,
      RegVarMap.fold (fun k -> fun v -> fun b -> RegVarMap.add k v b) sr1 sr2,
      EffVarMap.fold (fun k -> fun v -> fun b -> EffVarMap.add k v b) se1 se2
    )
end

module AnnotatedType = struct
  type t = annotated_type

  let rec ftv ty =
    match ty with
    | (TVar(s), _) -> TyVarSet.singleton s
    | (TArrow(t1, (ev, eff), t2), _) -> TyVarSet.union (ftv t1) (ftv t2)
    | (_, _) -> TyVarSet.empty ;;

  let rec frv ty =
    match ty with
    | (TArrow(t1, (ev, eff), t2), r) ->
       RegVarSet.union
         (RegVarSet.union (frv t1) (frv t2))
         (RegVarSet.union (RegVarSet.singleton r) (ArrowEff.frv (ev, eff)))
    | (_, r) -> RegVarSet.singleton r ;;

  let rec fev ty =
    match ty with
    | (TArrow(t1, (ev, eff), t2), _) ->
       EffVarSet.union
         (EffVarSet.union (fev t1) (fev t2))
         (EffVarSet.union (EffVarSet.singleton ev) (ArrowEff.fev (ev, eff)))
    | (_, _) -> EffVarSet.empty ;;

  let fv ty = ((ftv ty), (frv ty), (fev ty))

  let rec unify_arrow_effect (e1, eff1) (e2, eff2) =
    if EffVar.equal (e1, e2) then Substitute.empty
    else (
      let eff' = EffSet.union eff1 eff2 in
      (
        TyVarMap.empty,
        RegVarMap.empty,
        EffVarMap.add e2 (e2, eff') (EffVarMap.singleton e1 (e2, eff'))
      )
    )

  let rec unify_rho r1 r2 =
    if RegVar.equal (r1, r2) then Substitute.empty
    else (TyVarMap.empty, RegVarMap.singleton r1 r2, EffVarMap.empty)

  let rec unify ((t1, r1) as twp1) ((t2, r2) as twp2) =
    let sr = unify_rho r1 r2 in
    match (t1, t2) with
    | (TVar x, _) ->
       if TyVarSet.mem x (ftv twp2) then failwith "unify failed with occur"
       else Substitute.compose sr (TyVarMap.singleton x t2, RegVarMap.empty, EffVarMap.empty)
    | (_, TVar x) ->
       if TyVarSet.mem x (ftv twp1) then failwith "unify failed with occur"
       else Substitute.compose sr (TyVarMap.singleton x t1, RegVarMap.empty, EffVarMap.empty)
    | (TArrow(tp1, ae1, tp2), TArrow(tp3, ae2, tp4)) ->
       let s1 = unify tp1 tp2 in
       let s2 = unify tp3 tp4 in
       let s3 = unify_arrow_effect ae1 ae2 in
       Substitute.compose s3 (Substitute.compose s2 (Substitute.compose s1 sr))
    | (_, _) -> failwith "unify failed"
end

module IdentEnv = Map.Make(String) ;;

module VarStream = struct
  let intro = (0, 0, 0)
  let fresh_type_var (a, b, c) =
    (TVar(TyVar.new_var a), (a+1, b, c))
  let fresh_reg_var (a, b, c) =
    (RegVar.new_var b, (a, b+1, c))
  let fresh_eff_var (a, b, c) =
    (EffVar.new_var c, (a, b, c+1))
  let fresh_ty_with_place (a, b, c) =
    ((TVar(TyVar.new_var a), RegVar.new_var b), (a+1, b+1, c))
end

module type RegExp = sig
  type 'a t =
    | RInt of int * RegVar.t * 'a
    | RVarX of ident * 'a
    | RVarF of ident * RegVar.t list * RegVar.t * 'a
    | RLet of ident * 'a t * 'a t * 'a
    | RLetRec of ident * RegVar.t list * ident * 'a t * 'a t * 'a
    | RCall of 'a t * 'a t * 'a
    | RLam of ident * 'a t * RegVar.t * 'a
    | REmpty ;;
end

module RegExp = struct
  type 'a t =
    | RInt of int * RegVar.t * 'a
    | RVarX of ident * 'a
    | RVarF of ident * RegVar.t list * RegVar.t * 'a
    | RLet of ident * 'a t * 'a t * 'a
    | RLetRec of ident * RegVar.t list * ident * 'a t * 'a t * 'a
    | RCall of 'a t * 'a t * 'a
    | RLam of ident * 'a t * RegVar.t * 'a
    | REmpty ;;
end

module RRegExp = struct
  type t =
    | RInt of int * RegVar.t * (ty_with_place * EffSet.t)
    | RVarX of ident * (ty_with_place * EffSet.t)
    | RVarF of ident * RegVar.t list * RegVar.t * (ty_with_place * EffSet.t)
    | RLet of ident * t * t * (ty_with_place * EffSet.t)
    | RLetRec of ident * RegVar.t list * ident * t * t * (ty_with_place * EffSet.t)
    | RCall of t * t * (ty_with_place * EffSet.t)
    | RLam of ident * t * RegVar.t * (ty_with_place * EffSet.t)

  let ty_with_place e =
    match e with
    | RInt (_, _, (t, _)) -> t
    | RVarX (_, (t, _)) -> t
    | RVarF (_, _, _, (t, _)) -> t
    | RLet (_, _, _, (t, _)) -> t
    | RLetRec (_, _, _, _, _, (t, _)) -> t
    | RCall (_, _, (t, _)) -> t
    | RLam (_, _, _, (t, _)) -> t

  let effects e =
    match e with
    | RInt (_, _, (_, e)) -> e
    | RVarX (_, (_, e)) -> e
    | RVarF (_, _, _, (_, e)) -> e
    | RLet (_, _, _, (_, e)) -> e
    | RLetRec (_, _, _, _, _, (_, e)) -> e
    | RCall (_, _, (_, e)) -> e
    | RLam (_, _, _, (_, e)) -> e
end

module type TRANSLATOR = sig
  val translate : SrcExp.t -> RRegExp.t
end

module TargetTranslator : TRANSLATOR = struct

  let translate exp =
    let rec walk e env subst vs =
      match e with
      | SrcExp.IntLit(n) ->
         let (rv, vs1) = VarStream.fresh_reg_var vs in
         (
           env,
           subst,
           vs1,
           RRegExp.RInt(n, rv, ((TInt(n), rv), EffSet.singleton (EffPut(rv))))
         )
      | SrcExp.Var(s) ->
         let (t', r') = IdentEnv.find s env in
         (
           env,
           subst,
           vs,
           RRegExp.RVarX(s, ((t', r'), EffSet.empty))
         )

         (*(match IdentEnv.find s env with
         | None -> failwith "No match variable"
         | Some t ->
            (
              env,
              subst,
              vs,
              RegExp.RVarX(s, (t, EffSet.empty))
            )
         )*)
      | SrcExp.Lam(x, e1) ->
         let ((tv, rv), vs1) = VarStream.fresh_ty_with_place vs in
         let env1 = IdentEnv.add x (tv, rv) env in
         let (env2, subst1, vs2, reg_e1) = walk e1 env1 subst vs1 in
         let (rv', vs3) = VarStream.fresh_reg_var vs2 in
         let (ev', vs4) = VarStream.fresh_eff_var vs3 in
         let t1 = RRegExp.ty_with_place reg_e1 in
         let e1 = RRegExp.effects reg_e1 in
         let twp1 = Substitute.subst subst (tv, rv) in
         let arr_eff' = (ev', e1) in
         let env3 = IdentEnv.remove x env2 in
         (
           env3,
           subst1,
           vs4,
           RRegExp.RLam(x, reg_e1, rv', ((TArrow(twp1, arr_eff', t1), rv'), EffSet.singleton(EffPut(rv'))))
         )
      | SrcExp.Call(e1, e2) ->
         let (env1, subst1, vs1, reg_e1) = walk e1 env subst vs in
         let t1 = RRegExp.ty_with_place reg_e1 in
         let eff1 = RRegExp.effects reg_e1 in
         let (env2, subst2, vs2, reg_e2) = walk e2 env1 subst1 vs1 in
         let t2 = RRegExp.ty_with_place reg_e2 in
         let eff2 = RRegExp.effects reg_e2 in
         let (tv, vs3) = VarStream.fresh_type_var vs2 in
         let (t1', r1') = Substitute.subst subst2 t1 in
         let (subst3', (t', r'), (ev, eff)) =
           match t1' with
           | TArrow((t11, r11), (ev, eff), (t12, r12)) ->
              let subst3 = AnnotatedType.unify (t11, r11) t2 in
              (subst3, Substitute.subst subst3 (t12, r12), (ev, eff))
           | _ -> failwith "is not TArrow" in
         let env3 = IdentEnv.map (fun t -> Substitute.subst subst3' t) env2 in
         let subst4 = Substitute.compose subst3' (Substitute.compose subst2 subst1) in
         let eff' = EffSet.union eff (EffSet.union eff1 (EffSet.union eff2 (EffSet.of_list [EffVar(ev); EffGet(r1')]) )) in
         (
           env3,
           subst4,
           vs3,
           (* reg_e1, reg_e2 へ subst が必要な気がする *)
           RRegExp.RCall(reg_e1, reg_e2, ((t', r1'), eff'))
         )
      | _ -> failwith "unknown expression"
    in
    let (env', subst', vs', reg_exp) = walk exp IdentEnv.empty Substitute.empty VarStream.intro in
    reg_exp
    ;;
end
