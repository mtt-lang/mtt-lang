open Base
open Mtt
open Mtt.Ast
module MttPP = Mtt.PrettyPrinter

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
      | Expr.Unit | Expr.VarR _ | Expr.VarM _ -> empty
      | Expr.Fst { e } -> shrink_unary Expr.fst e
      | Expr.Snd { e } -> shrink_unary Expr.snd e
      | Expr.Pair { e1; e2 } -> shrink_binary Expr.pair e1 e2
      | Expr.Fun { idr; ty_id; body } -> shrink_unary (Expr.func idr ty_id) body
      | Expr.App { fe; arge } -> shrink_binary Expr.app fe arge
      | Expr.Box { e } -> shrink_unary Expr.box e
      | Expr.Let { idr; bound; body } ->
          shrink_binary (Expr.letc idr) bound body
      | Expr.Letbox { idm; boxed; body } ->
          shrink_binary (Expr.letbox idm) boxed body
  in
  QCheck.make generator ~print:print_ast ~shrink:shrink_ast

let test_preserving_syntax =
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
        match Mtt.ParserInterface.parse_from_string Term ast_string with
        | Ok ast -> ast
        | Error err_msg ->
            QCheck.Test.fail_reportf "Parse error: %s\nParser input: %s" err_msg
              ast_string
      in
      Expr.equal ast parsed_ast
      || QCheck.Test.fail_reportf "Parser input: %s" ast_string)

let test_minimum_parentheses =
  let buffer_size = 1024 in
  let buffer = Stdlib.Buffer.create buffer_size in
  let parens_pairs str =
    let _, pairs =
      let f index (stack, pairs) char =
        match char with
        | '(' -> (index :: stack, pairs)
        | ')' -> (
            match stack with
            | [] -> ([], []) (* unbalanced *)
            | i :: tl -> (tl, (i, index) :: pairs) )
        | _ -> (stack, pairs)
      in
      String.foldi str ~init:([], []) ~f
    in
    pairs
  in
  let remove_parens str (l, r) =
    let length = String.length str in
    let prefix = String.prefix str l in
    let middle =
      String.drop_suffix (String.drop_prefix str (l + 1)) (length - r)
    in
    let suffix = String.suffix str (length - r - 1) in
    String.concat ~sep:" " [ prefix; middle; suffix ]
  in
  let any_redundant str pairs =
    let ast =
      match Mtt.ParserInterface.parse_from_string Term str with
      | Ok ast -> ast
      | Error err_msg ->
          QCheck.Test.fail_reportf "Parse error: %s\nParser input: %s" err_msg
            str
    in
    let redundant (l, r) =
      let str' = remove_parens str (l, r) in
      match Mtt.ParserInterface.parse_from_string Term str' with
      | Ok ast' -> Expr.equal ast ast'
      | Error _ -> false
    in
    let rec f pairs =
      match pairs with
      | [] -> None
      | (l, r) :: tl -> if redundant (l, r) then Some (l, r) else f tl
    in
    f pairs
  in
  QCheck.Test.make ~name:"Expression pretty printer uses minimum parentheses"
    ~count:1000 ~long_factor:10 arbitrary_ast (fun ast ->
      let _ =
        try (PPrint.ToBuffer.pretty 1.0 80 buffer) (MttPP.Doc.of_expr ast)
        with _ -> ()
      in
      let ast_string = Stdlib.Buffer.contents buffer in
      let _ = Stdlib.Buffer.clear buffer in
      let pair = any_redundant ast_string (parens_pairs ast_string) in
      match pair with
      | Some (l, r) ->
          QCheck.Test.fail_reportf
            "Original expression:\n  %s\nSimplified expression:\n  %s"
            ast_string
            (remove_parens ast_string (l, r))
      | None -> true)

let _ =
  QCheck_runner.run_tests_main
    [ test_preserving_syntax; test_minimum_parentheses ]
