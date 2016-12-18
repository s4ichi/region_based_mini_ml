{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''

rule token = parse
  (* 整数定数 *)
  | digit+
    { let str = Lexing.lexeme lexbuf in
      INT (int_of_string str) }

  (* 演算子 *)
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { ASTERISK }
  | '/'       { SLASH }
  | '='       { EQUAL }
  | '<'       { LESS }
  | '>'       { GREATER }
  | "<>"       { NOTEQ }
  | "::"      { COLCOL }

  (* 括弧類 *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LBRA }
  | ']'       { RBRA }

  (* 区切り記号 *)
  | "->"      { ARROW }
  | '|'       { VBAR }

  (* キーワード *)
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "fun"     { FUN }
  | "let"     { LET }
  | "rec"     { REC }
  | "in"      { IN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "match"   { MATCH }
  | "with"    { WITH }
  | "List.hd" { HEAD }
  | "List.tl" { TAIL }

  (* 変数 *)
  | alpha alnum*
    { VAR (Lexing.lexeme lexbuf) }

  (* 制御記号 *)
  | eof       { EOF }

  (* スペースを読み飛ばす *)
  | space+    { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }
