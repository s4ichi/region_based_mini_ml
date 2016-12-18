(* main.ml *)

open Source ;;
open Target ;;

let parse str =
  Parser.main Lexer.token
    (Lexing.from_string str)
