(* source.ml *)

open Util

module SourceVal : Value = struct
  type t =
    | IntVal  of int
    | BoolVal  of bool
    | LamVal  of string * exp * env
    | RecFunVal of string * string * exp * Env.value
end

module SourceVar : Variable = struct
  type t = ident
  let equal (x, x') = x = x'
  let compare = fun (a,b) -> String.compare a b
end

module SourceEnv : Env = struct
  type key = SourceVar.t
  type value = SourceVar.t

  let empty = []

  let lookup env x =
    match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y,v)::tl -> if x=y then v
      else lookup x tl

  let insert env x v = (x,v) :: env ;;
end

module SourceTypeVal : Value = struct
  type t = TBool | TInt | TFunc | TApp
end

module SourceTypeVar : Variable = struct
  include SourceVar
end

module SourceTypeEnv : Env = struct
  type key = SourceTypeVar.t
  type value = SourceTypeVal.t

  let empty = []

  let lookup env x =
    match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y,v)::tl -> if x=y then v
      else lookup x tl

  let insert env x v = (x,v) :: env ;;
end

module SourceExp : Exp = struct
  type t =
    | IntLit of int
    | BoolLit of bool
    | Var of string
    | Eq of exp * exp       (* e = e *)
    | NotEq of exp * exp    (* e = e *)
    | Greater of exp * exp  (* e > e *)
    | Less of exp * exp     (* e < e *)
    | Plus of exp * exp     (* e + e *)
    | Minus of exp * exp    (* e - e *)
    | Times of exp * exp    (* e * e *)
    | Div of exp * exp      (* e / e *)
    | Let of string * exp * exp
    | LetRec of string * string * exp * exp
    | Call of exp * exp
    | Lam of string * exp ;;

  type ty = SourceTypeVal.t
  type value = SourceVal.t

  let type_check exp =
    let rec walk env e =
      match exp with
      | _ -> failwith "wrong value"
    in
    walk SourceTypeEnv.empty exp ;;

  let type_inf exp =
    match exp with
    | _ -> failwith "wrong value" ;;

  let rec eval e env =
    let binop f e1 e2 env =
      let eval_e2 = eval e2 env in
      let eval_e1 = eval e1 env in
      match (eval_e1, eval_e2) with
      | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
      | _ -> failwith "integer value expected"
    in
    let equal_val e1 e2 =
      let eval_e2 = eval e2 env in
      let eval_e1 = eval e1 env in
      match (eval_e1, eval_e2) with
      | (IntVal(n1),IntVal(n2)) -> n1 = n2
      | (BoolVal(b1),BoolVal(b2)) -> b1 = b2
      | _ -> failwith "wrong value"
    in
    let greater_val e1 e2 =
      let eval_e2 = eval e2 env in
      let eval_e1 = eval e1 env in
      match (eval_e1, eval_e2) with
      | (IntVal(n1),IntVal(n2)) -> n1 > n2
      | (BoolVal(b1),BoolVal(b2)) -> b1 > b2
      | _ -> failwith "wrong value"
    in
    match e with
    | Var(x)            -> lookup x env
    | IntLit(n)         -> IntVal(n)
    | BoolLit(b)        -> BoolVal(b)
    | Lam(x,e1)         -> LamVal(x, e1, env)
    | Call(e1,e2)        ->
       let arg = (eval e2 env) in
       let func = (eval e1 env) in
       (match func with
       | LamVal(x,body,env1) ->
          let env2 = (ext env1 x arg) in
          eval body env2
       | RecFunVal(f,x,body,env1) ->
          let env2 = (ext (ext env1 x arg) f func) in
          eval body env2
       | _ -> failwith "wrong value in App")
    | Plus(e1,e2)       -> binop ( + ) e1 e2 env
    | Times(e1,e2)      -> binop ( * ) e1 e2 env
    | Minus(e1,e2)      -> binop ( - ) e1 e2 env
    | Div(e1,e2)        ->
       binop (fun p1 p2 ->
         match p2 with
         | 0 -> failwith "Divide by zero exception"
         | _ -> p1 / p2 ) e1 e2 env
    | Eq(e1,e2)         -> BoolVal(equal_val e1 e2)
    | NotEq(e1,e2)      -> BoolVal(not (equal_val e1 e2))
    | Greater(e1,e2)    -> BoolVal(greater_val e1 e2)
    | Less(e1,e2)       -> BoolVal(not (greater_val e1 e2))
    | Let(x,e1,e2)      ->
       let env1 = ext env x (eval e1 env) in
       eval e2 env1
    | LetRec(f,x,e1,e2) ->
       let env1 = ext env f (RecFunVal (f, x, e1, env))
       in eval e2 env1 ;;
end
