(* source.ml *)

type ident = string

type exp =
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
  | Lam of string * exp
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

  type ty = TBool | TInt | TArrow of ty * ty

  type value =
    | IntVal  of int
    | BoolVal  of bool
    | LamVal  of string * exp * value Env.t
    | RecFunVal of string * string * exp * value Env.t

  let type_inf exp =
    match exp with
    | _ -> failwith "wrong value" ;;

  let  eval exp =
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
