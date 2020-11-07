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
           let regular_id id = Mtt.Id.R.mk id in
           let modal_id id = Mtt.Id.M.mk (id ^ "'") in
           match size with
           | 0 ->
               oneof
                 [
                   return Expr.Unit;
                   (* won't work until expressions with free variables can be pretty-printed *)
                   (* map
                      (fun s -> Expr.VarL (Mtt.Id.R.mk s))
                      (string_size ~gen:(char_range 'a' 'z') (return 1)); *)
                   map (fun idg -> Expr.VarG (modal_id idg)) lowercase_id;
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
  let shrink_ast =
    let open QCheck.Iter in
    function
    | Expr.Unit | Expr.VarL _ | Expr.VarG _ -> empty
    | Expr.Fst pe | Expr.Snd pe -> return pe
    | Expr.Pair (e1, e2) -> of_list [ e1; e2 ]
    | Expr.Fun (_, _, body) -> return body
    | Expr.App (fe, arge) -> of_list [ fe; arge ]
    | Expr.Box e -> return e
    | Expr.Let (_, bound_e, body) -> of_list [ bound_e; body ]
    | Expr.Letbox (_, boxed_e, body) -> of_list [ boxed_e; body ]
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
      Expr.equal ast parsed_ast)

let _ = QCheck_runner.run_tests_main [ test ]
