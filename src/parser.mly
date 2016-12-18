%{
open Source
%}

// リテラル
%token <string> VAR  // x, y, abc, ...
%token <int> INT     // 0, 1, 2, ...

// 演算子
%token PLUS     // '+'
%token MINUS    // '-'
%token ASTERISK // '*'
%token SLASH    // '/'
%token EQUAL    // '='
%token LESS     // '<'
%token GREATER  // '>'
%token NOTEQ    // '<>'
%token COLCOL   // "::"

// 括弧類
%token LPAREN   // '('
%token RPAREN   // ')'
%token LBRA     // '['
%token RBRA     // ']'

// 区切り記号
%token ARROW    // "->"
%token VBAR     // '|'

// キーワード
%token TRUE     // "true"
%token FALSE    // "false"
%token FUN      // "fun"
%token LET      // "let"
%token REC      // "rec"
%token IN       // "in"
%token IF       // "if"
%token THEN     // "then"
%token ELSE     // "else"
%token MATCH    // "match"
%token WITH     // "with"
%token HEAD     // "List.hd"
%token TAIL     // "List.tl"

// 制御記号
%token EOF

// 演算子優先順位 (優先度の低いものほど先)
%nonassoc IN ELSE ARROW WITH
%left VBAR
%left EQUAL GREATER LESS NOTEQ
%right COLCOL
%left PLUS MINUS
%left ASTERISK SLASH
%nonassoc UNARY
// 最後にarg_exprの一番左のトークンを並べる
%left VAR INT TRUE FALSE LBRA LPAREN

%start main
%type <Source.exp> main

%%

// 開始記号
main:
  | exp EOF
    { $1 }
;

// 関数の引数になれる式
arg_exp:
  | VAR
    { Var $1 }

  | INT
    { IntLit $1 }

  | TRUE
    { BoolLit true }

  | FALSE
    { BoolLit false }

  // 空リスト
  | LBRA RBRA
    { Empty }

  // 括弧で囲まれた式
  | LPAREN exp RPAREN
    { $2 }
;

// 式
exp:
  | arg_exp
    { $1 }

  // 関数適用 (e1 e2)
  | exp arg_exp
    { Call ($1, $2) }

  // 符号の反転 -e
  | MINUS exp %prec UNARY
    { Minus (IntLit 0, $2) }

  // e1 + e2
  | exp PLUS exp
    { Plus ($1, $3) }

  // e1 - e2
  | exp MINUS exp
    { Minus ($1, $3) }

  // e1 * e2
  | exp ASTERISK exp
    { Times ($1, $3) }

  // e1 / e2
  | exp SLASH exp
    { Div ($1, $3) }

  // e1 = e2
  | exp EQUAL exp
    { Eq ($1, $3) }

  // e1 < e2
  | exp LESS exp
    { Less ($1, $3) }

  // e1 > e2
  | exp GREATER exp
    { Greater ($1, $3) }

  // e1 <> e2
  | exp NOTEQ exp
    { NotEq ($1, $3) }

  // fun x -> e
  | FUN VAR ARROW exp
    { Lam ($2, $4) }

  // let x = e1 in e2
  | LET VAR EQUAL exp IN exp
    { Let ($2, $4, $6) }

  // let rec f x = e1 in e2
  | LET REC VAR VAR EQUAL exp IN exp
    { LetRec ($3, $4, $6, $8) }

  // if e1 then e2 else e3
  | IF exp THEN exp ELSE exp
    { If ($2, $4, $6) }

  | error
    {
      let message =
        Printf.sprintf
          "parse error near characters %d-%d"
          (Parsing.symbol_start ())
	        (Parsing.symbol_end ())
	    in
	    failwith message
	  }
;

// match文のcaseの列
// 注: yaccでは左再帰のほうがスタック消費量が少ない。
cases_rev:
  | pattern ARROW exp
    { [($1, $3)] }

  | cases_rev VBAR pattern ARROW exp
    { ($3, $5) :: $1 }
;

// パターン
pattern:
  | VAR
    { Var $1 }

  | INT
    { IntLit $1 }

  | TRUE
    { BoolLit true }

  | FALSE
    { BoolLit false }

  | LBRA RBRA
    { Empty }
;