(* (\* *)
(*   Util env module *)
(* *\) *)

(* type 'v env = (string * 'v) list ;; *)

(* let rec lookup env x = *)
(*   match env with *)
(*   | []        -> failwith (x ^ " not found") *)
(*   | (y, v)::r -> if x=y then v else lookup r x;; *)

(* (\* *)
(*   Runtime environment set. *)

(*   - region is finite map: offsets ->  strable values *)
(*   - strable value is (int or closure or region function closure) *)

(*   - variable environment is finite map: program variables -> values *)
(*   - region environment is is finite map: region variables -> region names *)

(*   - address is a pair (region name, offset) *)
(*   - store is finite map: region names -> regions *)
(*   - value is an address. *)
(* *\) *)

(* type offset = int ;; *)
(* type region_var = string ;; *)
(* type region_name = string ;; *)
(* type address = region_name * offset ;; *)
(* type value = address ;; *)

(* type var_env = value env ;; *)
(* type reg_env = region_name env ;; *)

(* type strable_value = *)
(*   | Int of int *)
(*   | Closure of string * expr * var_env * reg_env *)
(*   | RegClosure of string * expr * var_env * reg_env ;; *)

(* type region = strable_value env ;; *)
(* type store = region env ;; *)

(* (\* *)
(*   Grammer of region based meta language. *)

(*   TODO: Lexer *)
(* *\) *)
(* type expr = *)
(*   | IntLit of int * string                                       (\* c at rho *\) *)
(*   | Var of string                                                (\* x *\) *)
(*   | Prim of string * expr * expr * string                        (\* x + y at rho *\) *)
(*   | Let of string * expr * expr                                  (\* let x = e1 in e2 *\) *)
(*   | Letrec of string * string list * string * string expr * expr (\* letrec f[rho1, rho2](x) at rho = e1 in e2*\) *)
(*   | Letreg of string list * expr                                 (\* letregion rho1, rho2 in e *\) *)
(*   | Call of expr * expr                                          (\* e1 e2 # Apply *\) *)
(*   | Lam of string * expr * string ;;                             (\* lambda x.e at rho *\) *)

(* (\* *)
(*   evaluate *)
(* *\) *)

(* let rec evaluate (e: expr) (s: store) (ve: var_env) (re: reg_env) = *)
(*   match e with *)
(*   | _ -> failwith "unkown type expr" ;; *)
