%token EOF

(* Identifiers *)
%token <string> IDR
%token <string> IDM
%token <string> IDT

(* Arithmetic *)
%token <Nat.t> UINTZ

(* Arithmetic *)
%token PLUS
%token MINUS
(* multiplication is `CROSS` token *)
%token SLASH

(* Parentheses *)
%token LPAREN RPAREN
%token LANGLE RANGLE

(* Type-level syntax *)
%token CROSS (** Could be term-level *)
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
%token LET
%token LETBOX
%token IN

%left PLUS MINUS
%right ARROW   (* Type arrows associate to the right *)
%left CROSS SLASH   (* Type products have higher precedence than arrow types *)
                  (* Multiplication and division have greater priority *)
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
    { Location.locate_start_end (VarL (Id.R.mk name)) $symbolstartpos $endpos }

    (* Modal (i.e. valid) variables *)
  | name = IDM
    { Location.locate_start_end (VarG (Id.M.mk name)) $symbolstartpos $endpos }

expr:
    (* First projection *)
  | FST; e = parceled_expr
    { Location.locate_start_end (Fst (e)) $symbolstartpos $endpos }

    (* Second projection *)
  | SND; e = parceled_expr
    { Location.locate_start_end (Snd (e)) $symbolstartpos $endpos }

    (* anonymous function (lambda) *)
  | FUN; idr = IDR; COLON; t = typ; DARROW; e = expr
    { Location.locate_start_end (Fun (Id.R.mk idr, t, e)) $symbolstartpos $endpos }

    (* allow parenthesizing of the bound variable for lambdas *)
  | FUN; LPAREN; idr = IDR; COLON; t = typ; RPAREN; DARROW; e = expr
    { Location.locate_start_end (Fun (Id.R.mk idr, t, e)) $symbolstartpos $endpos }

    (* function application (f x) *)
  | fe = parceled_expr; arge = parceled_expr
    { Location.locate_start_end (App (fe, arge)) $symbolstartpos $endpos }

    (* term-level box *)
  | BOX; e = parceled_expr
    { Location.locate_start_end (Box e) $symbolstartpos $endpos }

    (* let idr = expr in expr *)
  | LET; idr = IDR; EQ; e = expr; IN; body = expr
    { Location.locate_start_end (Let (Id.R.mk idr, e, body)) $symbolstartpos $endpos }

    (* letbox idg = expr in expr *)
  | LETBOX; idg = IDM; EQ; e = expr; IN; body = expr
    { Location.locate_start_end (Letbox (Id.M.mk idg, e, body)) $symbolstartpos $endpos }
  
  | e = parceled_expr
    { e }
  

parceled_expr:
    (* Unit *)
  | UNIT
    { Location.locate_start_end Unit $symbolstartpos $endpos }

    (* Identifiers *)
  | i = ident
    { i }

    (* Parenthesized expressions *)
  | LPAREN; e = expr; RPAREN
    { e }

    (* Arithmetic *)
  | n = UINTZ
    { Location.locate_start_end (Nat n) $symbolstartpos $endpos }

    (* e1 + e2 *)
  | e1 = parceled_expr; PLUS; e2 = parceled_expr
    { Location.locate_start_end (BinOp (Add, e1, e2)) $symbolstartpos $endpos }

    (* e1 - e2 *)
  | e1 = parceled_expr; MINUS; e2 = parceled_expr
    { Location.locate_start_end (BinOp (Sub, e1, e2)) $symbolstartpos $endpos }

    (* e1 * e2 *)
  | e1 = parceled_expr; CROSS; e2 = parceled_expr
    { Location.locate_start_end (BinOp (Mul, e1, e2)) $symbolstartpos $endpos }

    (* e1 / e2 *)
  | e1 = parceled_expr; SLASH; e2 = parceled_expr
    { Location.locate_start_end (BinOp (Div, e1, e2)) $symbolstartpos $endpos }

    (* Pair of expressions *)
  | LANGLE; e1 = expr; COMMA; e2 = expr; RANGLE
    { Location.locate_start_end (Pair (e1, e2)) $symbolstartpos $endpos }

expr_eof:
  | e = expr; EOF { e }

type_eof:
  | t = typ; EOF { t }
