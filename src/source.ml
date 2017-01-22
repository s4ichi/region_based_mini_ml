(* source.ml *)

type ident = string

type exp =
  | IntLit of int
  | BoolLit of bool
  | Var of ident
  | Eq of exp * exp       (* e = e *)
  | NotEq of exp * exp    (* e = e *)
  | Greater of exp * exp  (* e > e *)
  | Less of exp * exp     (* e < e *)
  | Plus of exp * exp     (* e + e *)
  | Minus of exp * exp    (* e - e *)
  | Times of exp * exp    (* e * e *)
  | Div of exp * exp      (* e / e *)
  | Let of ident * exp * exp
  | LetRec of ident * ident * exp * exp
  | Call of exp * exp
  | Lam of ident * exp
  | If of exp * exp * exp
  | Empty

module type VARIABLE = sig
  type t
  val equal   : t * t -> bool
  val compare : t * t -> int
end

module SourceVar : VARIABLE = struct
  type t = ident
  let equal (x, x') = x = x'
  let compare = fun (a,b) -> String.compare a b
end

module SourceTypeVar : VARIABLE = struct
  include SourceVar
end

module type INTERPRETER = sig
  type value
  type ty

  val type_inf   : exp -> ty
  val eval       : exp -> value
end

module SourceInterpreter : INTERPRETER = struct
  module Env = Map.Make(String) ;;
  module TypeEnv = Map.Make(String) ;;
  module TypeSubst = Map.Make(String) ;;

  type ty = TBool | TInt | TArrow of ty * ty

  type value =
    | IntVal  of int
    | BoolVal  of bool
    | LamVal  of ident * exp * value Env.t
    | RecFunVal of ident * ident * exp * value Env.t

  let new_typevar n =
    (TVar ("'a" ^ (string_of_int n)), n+1)

  (* 型環境 te1 における 型変数t1 を TInt に変更した型環境 *)
  let rec substitute tvar t te =
    List.map (fun (x,t2) -> if t2 = tvar then (x,t) else (x,t2)) te

  let rec substitute tvar t te =
    TypeEnv.mapi (fun (k,v) -> if v = tvar then (k,t) else (k,v)) te

  let rec occurs tx t =
    if tx = t then true
    else
      match t with
      | TArrow(t1,t2) -> (occurs tx t1) || (occurs tx t2)
      | _ -> false

  (* subst_ty : tysubst -> ty -> ty *)
  (* 代入thetaを型t に適用する *)
  let rec subst_ty theta t =
    let rec subst_ty1 theta1 s =
      match theta1 with
      |	[] -> TVar(s)
      | (tx,t1):: theta2 ->
	       if tx = s then t1
	       else subst_ty1 theta2 s
    in match t with
    |  TInt -> TInt
    | TBool -> TBool
    | TArrow(t2,t3) -> TArrow(subst_ty theta t2, subst_ty theta t3)
    | TVar(s) -> subst_ty1 theta s

  let rec apply_ty theta t =
    match t with
    | TInt -> TInt
    | TBool -> TBool
    | TArrow(t1, t2) -> TArrow(apply_ty theta t1, apply_ty theta t2)
    | TVar(s) ->
       if TypeSubst.exists (fun k v -> k = s) theta
       then TypeSubst.find s theta
       else TVar(s)

  (* subst_tyenv  : tysubst -> tyenv -> tyenv *)
  (* 代入thetaを型環境 te に適用する *)
  let subst_tyenv theta te =
    List.map (fun (x,t) -> (x, subst_ty theta t)) te
  (* List.mapは OCaml における リストに対する iterator である *)

  let apply_tyenv theta te =
    TypeEnv.map (fun v -> apply_ty theta v) te

  (* subst_eq : tysubst -> (ty * ty) list -> (ty * ty) list *)
  (* 代入thetaを型の等式のリスト eql に適用する *)
  let subst_eql theta eql =
    List.map (fun (t1,t2) -> (subst_ty theta t1, subst_ty theta t2))
	    eql

  let apply_eql theta eql =
    List.map (fun (t1,t2) -> (apply_ty theta t1, apply_ty theta t2)) eql

  (* compose_subst : tysubst -> tysubst -> tysubst *)
  (* 2つの代入を合成した代入を返す。theta1 が「先」でtheta2が「後」である *)
  let rec compose_subst theta2 theta1 =
    (*各要素は適用したものを入れる*)
    let theta11 =
      List.map (fun (tx,t) -> (tx, subst_ty theta2 t)) theta1
    in
    (* 無いものを入れる *)
    List.fold_left (fun tau -> fun (tx,t) ->
		  try
			  let _ = lookup tx theta1 in
			  tau
		  with Failure(_) ->
			  (tx,t) :: tau)
      theta11
      theta2

  let rec merge_subst theta2 theta1 =
    TypeSubst.merge (fun k t1 t2 ->
      match (t1, t2)z with
      | (Some x, Some y)
      | (Some x, None  ) -> apply_ty theta2 x
      | (None  , Some x) -> x
    ) thenta1 theta2

  (* unify : Map.bindings -> tysubst -> tysubst *)
  let unify eql =
    let rec solve eql theta =
      match eql with
      | [] -> theta
      | (t1,t2):: eql2 ->
	       if t1 = t2 then solve eql2 theta
	       else
           begin
             match (t1,t2) with
	           | (TArrow(t11,t12),TArrow(t21,t22))
	             -> solve ((t11,t21)::(t12,t22)::eql2) theta
	           | (TVar(s), _)
	             -> if (occurs t1 t2) then failwith "unification failed"
	               else solve (subst_eql [(s,t2)] eql2)
	                 (compose_subst [(s,t2)] theta)
	           | (_,TVar(s))
	             -> if (occurs t2 t1) then failwith "unification failed"
	               else solve (subst_eql [(s,t1)] eql2)
	                 (compose_subst [(s,t1)] theta)
	           | (_,_) -> failwith "unification failed"
           end
    in solve eql TypeSubst.empty

  let type_inf te e n =
    match e with
    | Var(s) ->
	     (try
	        let t1 = TypeEnv.find s te in (te, t1, theta0, n)
	      with Failure(_) ->
	        let (tx,n1) = new_typevar n in
	        let te1 = TypeEnv.add s tx te in
	        (te1, tx, theta0, n1))
    | IntLit(_)   -> (te, TInt, theta0, n)
    | BoolLit(_)  -> (te, TBool, theta0, n)
    | Plus(e1,e2) ->
	     let (te1, t1, theta1, n1) = tinf te e1 n in
	     let (te2, t2, theta2, n2) = tinf te1 e2 n1 in
	     let t11 = subst_ty theta2 t1 in
	     let theta3 = unify [(t11,TInt); (t2,TInt)] in
	     let te3 = subst_tyenv theta3 te2 in
	     let theta4 = compose_subst theta3
	       (compose_subst theta2 theta1) in
	     (te3, TInt, theta4, n2)
    | Fun(x,e) ->
	     let (tx,n1) = new_typevar n in
	     let te1 = TypeEnv.add x tx te in
	     let (te2, t1, theta1, n2) = tinf te1 e n1 in
	     let t2 = subst_ty theta1 tx in
	     let te3 = TypeEnv.remove x te2 in
	     (te3, TArrow(t2, t1), theta1, n2)
    | App(e1,e2) ->
	     let (te1, t1, theta1, n1) = tinf te e1 n in
	     let (te2, t2, theta2, n2) = tinf te1 e2 n1 in
	     let (tx,n3) = new_typevar n2 in
	     let t11 = subst_ty theta2 t1 in
	     let theta3 = unify [(t11,TArrow(t2,tx))] in
	     let t3 = subst_ty theta3 tx in
	     let te3 = subst_tyenv theta3 te2 in
	     let theta4 = compose_subst theta3
	       (compose_subst theta2 theta1) in
	     (te3, t3, theta4, n3)
    | _ -> failwith "unknown expression"

  let eval exp =
    let rec walk e env =
      let binop f e1 e2 =
        let walk_e2 = walk e2 env in
        let walk_e1 = walk e1 env in
        match (walk_e1, walk_e2) with
        | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
        | _ -> failwith "integer value expected"
      in
      let equal_val e1 e2 =
        let walk_e2 = walk e2 env in
        let walk_e1 = walk e1 env in
        match (walk_e1, walk_e2) with
        | (IntVal(n1),IntVal(n2)) -> n1 = n2
        | (BoolVal(b1),BoolVal(b2)) -> b1 = b2
        | _ -> failwith "wrong value"
      in
      let greater_val e1 e2 =
        let walk_e2 = walk e2 env in
        let walk_e1 = walk e1 env in
        match (walk_e1, walk_e2) with
        | (IntVal(n1),IntVal(n2)) -> n1 > n2
        | (BoolVal(b1),BoolVal(b2)) -> b1 > b2
        | _ -> failwith "wrong value"
      in
      match e with
      | Var(x)            -> Env.find x env
      | IntLit(n)         -> IntVal(n)
      | BoolLit(b)        -> BoolVal(b)
      | Lam(x,e1)         -> LamVal(x, e1, env)
      | Call(e1,e2)       ->
         let arg = (walk e2 env) in
         let func = (walk e1 env) in
         (match func with
         | LamVal(x,body,env1) ->
            let env2 = Env.add x arg env1 in
            walk body env2
         | RecFunVal(f,x,body,env1) ->
            let env2 = Env.add f func (Env.add x arg env1) in
            walk body env2
         | _ -> failwith "wrong value in App")
      | Plus(e1,e2)       -> binop ( + ) e1 e2
      | Times(e1,e2)      -> binop ( * ) e1 e2
      | Minus(e1,e2)      -> binop ( - ) e1 e2
      | Div(e1,e2)        ->
         binop (fun p1 p2 ->
           match p2 with
           | 0 -> failwith "Divide by zero exception"
           | _ -> p1 / p2 ) e1 e2
      | Eq(e1,e2)         -> BoolVal(equal_val e1 e2)
      | NotEq(e1,e2)      -> BoolVal(not (equal_val e1 e2))
      | Greater(e1,e2)    -> BoolVal(greater_val e1 e2)
      | Less(e1,e2)       -> BoolVal(not (greater_val e1 e2))
      | Let(x,e1,e2)      ->
         let env1 = Env.add x (walk e1 env) env in
         walk e2 env1
      | LetRec(f,x,e1,e2) ->
         let env1 = Env.add f (RecFunVal (f, x, e1, env)) env in
         walk e2 env1
      | _                 -> failwith "wrong exp"
    in
    walk exp Env.empty ;;
end
