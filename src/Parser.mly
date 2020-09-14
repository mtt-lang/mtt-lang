%token EOF

(* Identifiers *)
%token <string> IDR
%token <string> IDM
%token <string> IDT

(* Parentheses *)
%token LPAREN RPAREN
%token LANGLE RANGLE

(* Type-level syntax *)
%token CROSS
%token TBOX
%token COLON
%token ARROW

%token COMMA
%token DARROW
%token EQ
%token UNIT (* Both term-level and type-level *)

(* Keywords *)
%token FST
%token SND
%token FUN
%token BOX
%token LETBOX
%token IN

%right ARROW   (* Type arrows associate to the right *)
%left CROSS    (* Type products have higher precedence than arrow types *)
%nonassoc TBOX (* Highest precedence *)

%{
  open Ast
  open Ast.Expr
%}

%start <Ast.Expr.t> expr_eof
%start <Ast.Type.t> type_eof

%%

typ:
    (* Unit type *)
  | UNIT
    { Type.Unit }

    (* Uninterpreted base types *)
  | name = IDT
    { Type.Base name }

    (* Type of pairs *)
  | t1 = typ; CROSS; t2 = typ
    { Type.Prod (t1, t2) }

    (* Type of functions *)
  | dom = typ; ARROW; cod = typ
    { Type.Arr (dom, cod) }

    (* Type-level box *)
  | TBOX; t = typ
    { Type.Box t }

    (* Parenthesized type expressions *)
  | LPAREN; t = typ; RPAREN
    { t }

ident:
    (* Regular variables *)
  | name = IDR
    { VarL (Id.R.mk name) }

    (* Modal (i.e. valid) variables *)
  | name = IDM
    { VarG (Id.M.mk name) }

expr:
    (* First projection *)
  | FST; e = parceled_expr
    { Fst (e) }

    (* Second projection *)
  | SND; e = parceled_expr
    { Snd (e) }

    (* anonymous function (lambda) *)
  | FUN; idl = IDR; COLON; t = typ; DARROW; e = expr
    { Fun (Id.R.mk idl, t, e) }

    (* allow parenthesizing of the bound variable for lambdas *)
  | FUN; LPAREN; idl = IDR; COLON; t = typ; RPAREN; DARROW; e = expr
    { Fun (Id.R.mk idl, t, e) }

    (* function application (f x) *)
  | fe = parceled_expr; arge = parceled_expr
    { App (fe, arge) }

    (* term-level box *)
  | BOX; e = parceled_expr
    { Box e }

    (* letbox idg = expr in expr *)
  | LETBOX; idg = IDM; EQ; e = expr; IN; body = expr
    { Letbox (Id.M.mk idg, e, body) }

  | e = parceled_expr
    { e }

parceled_expr:
    (* Unit *)
  | UNIT
    { Unit }

    (* Identifiers *)
  | i = ident
    { i }

    (* Parenthesized expressions *)
  | LPAREN; e = expr; RPAREN
    { e }

    (* Pair of expressions *)
  | LANGLE; e1 = expr; COMMA; e2 = expr; RANGLE
    { Pair (e1, e2) }

expr_eof:
  | e = expr; EOF { e }

type_eof:
  | t = typ; EOF { t }
