%token EOF

(* Identifiers *)
%token <string> IDR
%token <string> IDM
%token <string> IDT

(* Arithmetic *)
%token <Nat.t> UINTZ

(* Arithmetic *)
%token ZERO SUCC (* for pattern-matching *)
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
%token TNAT
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
%token FIX
%token BOX
%token LET
%token LETBOX
%token IN
%token MATCH WITH PIPE END

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

    (* Type of Nat (UIntZ now) *)
  | TNAT
    { Type.Nat }

    (* Uninterpreted base types *)
  | idt = IDT
    { Type.Base {idt} }

    (* Type of pairs *)
  | ty1 = typ; CROSS; ty2 = typ
    { Type.Prod {ty1; ty2} }

    (* Type of functions *)
  | dom = typ; ARROW; cod = typ
    { Type.Arr {dom; cod} }

    (* Type-level box *)
  | TBOX; ty = typ
    { Type.Box {ty} }

    (* Parenthesized type expressions *)
  | LPAREN; ty = typ; RPAREN
    { ty }

ident:
    (* Regular variables *)
  | name = IDR
    { Location.locate_start_end (VarR {idr = Id.R.mk name}) $symbolstartpos $endpos }

    (* Modal (i.e. valid) variables *)
  | name = IDM
    { Location.locate_start_end (VarM {idm = Id.M.mk name}) $symbolstartpos $endpos }

expr:
    (* First projection *)
  | FST; e = parceled_expr
    { Location.locate_start_end (Fst {e}) $symbolstartpos $endpos }

    (* Second projection *)
  | SND; e = parceled_expr
    { Location.locate_start_end (Snd {e}) $symbolstartpos $endpos }

    (* anonymous function (lambda) *)
  | FUN; idr = IDR; COLON; ty_id = typ; DARROW; body = expr
    { Location.locate_start_end (Fun {idr = Id.R.mk idr; ty_id; body}) $symbolstartpos $endpos }

    (* allow parenthesizing of the bound variable for lambdas *)
  | FUN; LPAREN; idr = IDR; COLON; ty_id = typ; RPAREN; DARROW; body = expr
    { Location.locate_start_end (Fun {idr = Id.R.mk idr; ty_id; body}) $symbolstartpos $endpos }

  | FIX; idr = IDR; COLON; t = typ; DARROW; f = IDR; e = expr
    { Location.locate_start_end (Fix (Id.R.mk idr, t, Id.R.mk f, e)) $symbolstartpos $endpos }

    (* function application (f x) *)
  | fe = parceled_expr; arge = parceled_expr
    { Location.locate_start_end (App {fe; arge}) $symbolstartpos $endpos }

    (* term-level box *)
  | BOX; e = parceled_expr
    { Location.locate_start_end (Box {e}) $symbolstartpos $endpos }

    (* let idr = expr in expr *)
  | LET; idr = IDR; EQ; bound = expr; IN; body = expr
    { Location.locate_start_end (Let {idr = Id.R.mk idr; bound; body}) $symbolstartpos $endpos }

    (* letbox idm = expr in expr *)
  | LETBOX; idm = IDM; EQ; boxed = expr; IN; body = expr
    { Location.locate_start_end (Letbox {idm = Id.M.mk idm; boxed; body}) $symbolstartpos $endpos }

    (* match expr with ... end *)
  | MATCH; matched = expr; WITH; PIPE; ZERO; DARROW; zbranch = expr; PIPE; SUCC; pred = IDR; DARROW; sbranch = expr; END
    { Location.locate_start_end (Match {matched; zbranch; pred = Id.R.mk pred; sbranch}) $symbolstartpos $endpos }

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
    { Location.locate_start_end (Nat {n}) $symbolstartpos $endpos }

    (* e1 + e2 *)
  | e1 = parceled_expr; PLUS; e2 = parceled_expr
    { Location.locate_start_end (BinOp {op = Add; e1; e2}) $symbolstartpos $endpos }

    (* e1 - e2 *)
  | e1 = parceled_expr; MINUS; e2 = parceled_expr
    { Location.locate_start_end (BinOp {op = Sub; e1; e2}) $symbolstartpos $endpos }

    (* e1 * e2 *)
  | e1 = parceled_expr; CROSS; e2 = parceled_expr
    { Location.locate_start_end (BinOp {op = Mul; e1; e2}) $symbolstartpos $endpos }

    (* e1 / e2 *)
  | e1 = parceled_expr; SLASH; e2 = parceled_expr
    { Location.locate_start_end (BinOp {op = Div; e1; e2}) $symbolstartpos $endpos }

    (* Pair of expressions *)
  | LANGLE; e1 = expr; COMMA; e2 = expr; RANGLE
    { Location.locate_start_end (Pair {e1; e2}) $symbolstartpos $endpos }

expr_eof:
  | e = expr; EOF { e }

type_eof:
  | t = typ; EOF { t }
