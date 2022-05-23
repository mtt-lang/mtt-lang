open Base
open Mtt_lib
open Mtt_lib.Ast
module MttPP = Mtt_lib.PrettyPrinter

(* QCheck generator for the arbitrary (and most likely invalid) expressions *)
let generator =
  QCheck.Gen.(
    sized
    @@ fix (fun self size ->
           let lowercase_id =
             string_size ~gen:(char_range 'a' 'z') (return 1)
           in
           let regular_id id = Id.R.mk id in
           let modal_id id = Id.M.mk (id ^ "'") in
           match size with
           | 0 ->
               oneof
                 [
                   return Expr.unit;
                   map (fun n -> Expr.nat (Nat.of_int n)) big_nat;
                   map (fun idr -> Expr.var_r (regular_id idr)) lowercase_id;
                   map (fun idm -> Expr.var_m (modal_id idm)) lowercase_id;
                 ]
           | size ->
               let open Expr in
               let unary_node f = map f (self (size - 1)) in
               let binary_node f = map2 f (self (size / 2)) (self (size / 2)) in
               oneof
                 [
                   binary_node pair;
                   unary_node fst;
                   unary_node snd;
                   binary_node app;
                   map3 func
                     (map regular_id lowercase_id)
                     (return Type.Unit)
                     (self (size - 1));
                   unary_node box;
                   binary_node (binop Add);
                   binary_node (binop Sub);
                   binary_node (binop Mul);
                   binary_node (binop Div);
                   map3 letc
                     (map regular_id lowercase_id)
                     (self (size / 2))
                     (self (size / 2));
                   map3 letbox
                     (map modal_id lowercase_id)
                     (self (size / 2))
                     (self (size / 2));
                   map3 match_with
                     (self (size / 3))
                     (self (size / 3))
                     (map regular_id lowercase_id)
                   <*> self (size / 3);
                 ]))

let arbitrary_ast =
  let print_ast t = [%sexp_of: Expr.t] t |> Sexp.to_string_hum in
  let rec shrink_ast =
    let open QCheck.Iter in
    let shrink_unary cons arg =
      return arg <+> (shrink_ast arg >|= fun arg' -> cons arg')
    in
    let shrink_binary cons arg1 arg2 =
      of_list [ arg1; arg2 ]
      <+> (shrink_ast arg1 >|= fun arg1' -> cons arg1' arg2)
      <+> (shrink_ast arg2 >|= fun arg2' -> cons arg1 arg2')
    in
    let shrink_ternary cons arg1 arg2 arg3 =
      of_list [ arg1; arg2; arg3 ]
      <+> (shrink_ast arg1 >|= fun arg1' -> cons arg1' arg2 arg3)
      <+> (shrink_ast arg2 >|= fun arg2' -> cons arg1 arg2' arg3)
      <+> (shrink_ast arg3 >|= fun arg3' -> cons arg1 arg2 arg3')
    in
    fun Mtt_lib.Location.{ data = expr; _ } ->
      match expr with
      | Expr.Unit | Expr.VarR _ | Expr.VarM _ -> empty
      | Expr.Fst { e } -> shrink_unary Expr.fst e
      | Expr.Snd { e } -> shrink_unary Expr.snd e
      | Expr.Pair { e1; e2 } -> shrink_binary Expr.pair e1 e2
      | Expr.Nat _ -> empty
      | Expr.BinOp { op; e1; e2 } -> shrink_binary (Expr.binop op) e1 e2
      | Expr.Fun { idr; ty_id; body } -> shrink_unary (Expr.func idr ty_id) body
      | Expr.App { fe; arge } -> shrink_binary Expr.app fe arge
      | Expr.Box { e } -> shrink_unary Expr.box e
      | Expr.Let { idr; bound; body } ->
          shrink_binary (Expr.letc idr) bound body
      | Expr.Letbox { idm; boxed; body } ->
          shrink_binary (Expr.letbox idm) boxed body
      | Expr.Match { matched; zbranch; pred; sbranch } ->
          shrink_ternary
            (fun m z s -> Expr.match_with m z pred s)
            matched zbranch sbranch
  in
  QCheck.make generator ~print:print_ast ~shrink:shrink_ast

let test =
  let buffer_size = 1024 in
  let buffer = Stdlib.Buffer.create buffer_size in
  QCheck.Test.make ~name:"Expression pretty printer preserving syntax"
    ~count:1000 ~long_factor:10 arbitrary_ast (fun ast ->
      let _ =
        try (PPrint.ToBuffer.pretty 1.0 80 buffer) (MttPP.Doc.of_expr ast)
        with _ -> ()
      in
      let ast_string = Stdlib.Buffer.contents buffer in
      let _ = Stdlib.Buffer.clear buffer in
      let parsed_ast =
        match Mtt_lib.ParserInterface.parse_from_string Term ast_string with
        | Ok ast -> ast
        | Error err_msg ->
            QCheck.Test.fail_reportf "Parse error: %s\nParser input: %s" err_msg
              ast_string
      in
      Expr.equal ast parsed_ast
      || QCheck.Test.fail_reportf "Parser input: %s" ast_string)

let _ = QCheck_runner.run_tests_main [ test ]
