%token EOF

(* Identifiers *)
%token <string> IDR
%token <string> IDM
%token <string> IDT

(* Parentheses *)
%token LPAREN RPAREN
(* this implementation different from this? :
%token LPAREN 
%token RPAREN
*)
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
    { Location.locate Type.Unit $symbolstartpos $endpos }

    (* Uninterpreted base types *)
  | name = IDT
    { Location.locate (Type.Base name) $symbolstartpos $endpos }

    (* Type of pairs *)
  | t1 = typ; CROSS; t2 = typ
    { Location.locate (Type.Prod (t1, t2)) $symbolstartpos $endpos }

    (* Type of functions *)
  | dom = typ; ARROW; cod = typ
    { Location.locate (Type.Arr (dom, cod)) $symbolstartpos $endpos }

    (* Type-level box *)
  | TBOX; t = typ
    { Location.locate (Type.Box t) $symbolstartpos $endpos }

    (* Parenthesized type expressions *)
  | LPAREN; t = typ; RPAREN
    {t}

ident:
    (* Regular variables *)
  | name = IDR
    { Location.locate (VarL (Id.R.mk name)) $symbolstartpos $endpos }

    (* Modal (i.e. valid) variables *)
  | name = IDM
    { Location.locate (VarG (Id.M.mk name)) $symbolstartpos $endpos }

expr:
    (* First projection *)
  | FST; e = parceled_expr
    { Location.locate (Fst (e)) $symbolstartpos $endpos }

    (* Second projection *)
  | SND; e = parceled_expr
    { Location.locate (Snd (e)) $symbolstartpos $endpos }

    (* anonymous function (lambda) *)
  | FUN; idl = IDR; COLON; t = typ; DARROW; e = expr
    { Location.locate (Fun (Id.R.mk idl, t, e)) $symbolstartpos $endpos }

    (* allow parenthesizing of the bound variable for lambdas *)
  | FUN; LPAREN; idl = IDR; COLON; t = typ; RPAREN; DARROW; e = expr
    { Location.locate (Fun (Id.R.mk idl, t, e)) $symbolstartpos $endpos }

    (* function application (f x) *)
  | fe = parceled_expr; arge = parceled_expr
    { Location.locate (App (fe, arge)) $symbolstartpos $endpos }

    (* term-level box *)
  | BOX; e = parceled_expr
    { Location.locate (Box e) $symbolstartpos $endpos }

    (* letbox idg = expr in expr *)
  | LETBOX; idg = IDM; EQ; e = expr; IN; body = expr
    { Location.locate (Letbox (Id.M.mk idg, e, body)) $symbolstartpos $endpos }

  | e = parceled_expr
    { e }

parceled_expr:
    (* Unit *)
  | UNIT
    { Location.locate Unit $symbolstartpos $endpos }

    (* Identifiers *)
  | i = ident
    { i }

    (* Parenthesized expressions *)
  | LPAREN; e = expr; RPAREN
    { e }

    (* Pair of expressions *)
  | LANGLE; e1 = expr; COMMA; e2 = expr; RANGLE
    { Location.locate (Pair (e1, e2)) $symbolstartpos $endpos }

expr_eof:
  | e = expr; EOF { e }

type_eof:
  | t = typ; EOF { t }
