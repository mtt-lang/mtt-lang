open Base
open Mtt.Ast
open Mtt.PrettyPrinter

(* QCheck generator for the arbitrary (and most likely invalid) expressions *)
let generator =
  QCheck.Gen.(
    sized
    @@ fix (fun self size ->
           let lowercase_id =
             string_size ~gen:(char_range 'a' 'z') (return 1)
           in
           let open Expr in
           let regular_id id = Mtt.Id.R.mk id in
           let modal_id id = Mtt.Id.M.mk (id ^ "'") in
           match size with
           | 0 ->
               oneof
                 [
                   return unit;
                   (* won't work until expressions with free variables can be pretty-printed *)
                   (* map
                      (fun s -> Expr.VarL (Mtt.Id.R.mk s))
                      (string_size ~gen:(char_range 'a' 'z') (return 1)); *)
                   map (fun idg -> varg (modal_id idg)) lowercase_id;
                 ]
           | size ->
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
                     (return Type.unit)
                     (self (size - 1));
                   unary_node box;
                   map3 letc
                     (map regular_id lowercase_id)
                     (self (size / 2))
                     (self (size / 2));
                   map3 letbox
                     (map modal_id lowercase_id)
                     (self (size / 2))
                     (self (size / 2));
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
    fun Mtt.Location.{ data = expr; _ } ->
      match expr with
      | Expr.Unit | Expr.VarL _ | Expr.VarG _ -> empty
      | Expr.Fst pe -> shrink_unary Expr.fst pe
      | Expr.Snd pe -> shrink_unary Expr.snd pe
      | Expr.Pair (e1, e2) -> shrink_binary Expr.pair e1 e2
      | Expr.Fun (idl, t_of_id, body) ->
          shrink_unary (Expr.func idl t_of_id) body
      | Expr.App (fe, arge) -> shrink_binary Expr.app fe arge
      | Expr.Box e -> shrink_unary Expr.box e
      | Expr.Let (idl, bound_e, body) ->
          shrink_binary (Expr.letc idl) bound_e body
      | Expr.Letbox (idg, boxed_e, body) ->
          shrink_binary (Expr.letbox idg) boxed_e body
  in
  QCheck.make generator ~print:print_ast ~shrink:shrink_ast

let test =
  let buffer_size = 1024 in
  let buffer = Stdlib.Buffer.create buffer_size in
  QCheck.Test.make ~name:"Expression pretty printer preserving syntax"
    ~count:1000 ~long_factor:10 arbitrary_ast (fun ast ->
      let _ =
        try (PPrint.ToBuffer.pretty 1.0 80 buffer) (Doc.of_expr ast)
        with _ -> ()
      in
      let ast_string = Stdlib.Buffer.contents buffer in
      let _ = Stdlib.Buffer.clear buffer in
      let parsed_ast =
        match Mtt.ParserInterface.parse_from_string Term ast_string with
        | Ok ast -> ast
        | Error err_msg ->
            QCheck.Test.fail_reportf "Parse error: %s\nParser input: %s" err_msg
              ast_string
      in
      Expr.equal ast parsed_ast
      || QCheck.Test.fail_reportf "Parser input: %s" ast_string)

let _ = QCheck_runner.run_tests_main [ test ]
