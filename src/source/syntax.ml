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
  | Lam of string * exp ;;

type value =
  | IntVal  of int
  | BoolVal  of bool
  | LamVal  of string * exp * env
  | RecFunVal of string * string * exp * env
and
  env = (string * value) list
