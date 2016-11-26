open Syntax ;;

type typ = TBool | TInt | TFunc | TApp ;;
type typ_env = (string * typ) list ;;

let type_check env exp =
  match exp with
  | _ -> failwith "wrong value" ;;

let type_inf exp =
  match exp with
  | _ -> failwith "wrong value" ;;
