type src_expr =
  | IntLit of int * string                                       (* c at rho *)
  | Var of string                                                (* x *)
  | Prim of string * expr * expr * string                        (* x + y at rho *)
  | Let of string * expr * expr                                  (* let x = e1 in e2 *)
  | Letrec of string * string list * string * string expr * expr (* letrec f[rho1, rho2](x) at rho = e1 in e2*)
  | Letreg of string list * expr                                 (* letregion rho1, rho2 in e *)
  | Call of expr * expr                                          (* e1 e2 # Apply *)
  | Lam of string * expr * string ;;                             (* lambda x.e at rho *)
