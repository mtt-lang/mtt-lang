open Base
open Mtt.Ast
open Mtt.PrettyPrinter

(* QCheck generator for the arbitrary (and most likely invalid) expressions *)
let generator =
  (* let max_size = 1000 in *)
  QCheck.Gen.(
    sized
    (* sized_size (int_bound max_size) *)
    @@ fix (fun self size ->
           let lowercase_id =
             string_size ~gen:(char_range 'a' 'z') (return 1)
           in
           let modal_id id = Mtt.Id.M.mk (id ^ "'") in
           match size with
           | 0 ->
               oneof
                 [
                   return Expr.Unit;
                   (* won't work until expressions with free variables can be parsed *)
                   (* map
                      (fun s -> Expr.VarL (R.mk s))
                      (string_size ~gen:(char_range 'a' 'z') (return 1)); *)
                   map (fun idg -> Expr.VarG (modal_id idg)) lowercase_id;
                 ]
           | size ->
               let unary_node f = map f (self (size - 1)) in
               let binary_node f = map2 f (self (size / 2)) (self (size / 2)) in
               oneof
                 [
                   binary_node (fun e1 e2 -> Expr.Pair (e1, e2));
                   unary_node (fun pe -> Expr.Fst pe);
                   unary_node (fun pe -> Expr.Snd pe);
                   binary_node (fun x y -> Expr.App (x, y));
                   map3
                     (fun x y z -> Expr.Fun (x, y, z))
                     (map (fun s -> Mtt.Id.R.mk s) lowercase_id)
                     (return Type.Unit)
                     (self (size - 1));
                   unary_node (fun x -> Expr.Box x);
                   map3
                     (fun x y z -> Expr.Letbox (x, y, z))
                     (map modal_id lowercase_id)
                     (self (size / 2))
                     (self (size / 2));
                 ]))

(* Primitive expressions printer for testing purposes *)
let print_ast t = [%sexp_of: Expr.t] t |> Sexp.to_string

let arbitrary_ast = QCheck.make generator ~print:print_ast

let test =
  let buffer_size = 1024 in
  let buffer = Stdlib.Buffer.create buffer_size in
  QCheck.Test.make ~name:"Expression pretty printer preserving syntax"
    ~count:1000 arbitrary_ast (fun ast ->
      let _ =
        try (PPrint.ToBuffer.pretty 1.0 80 buffer) (Doc.of_expr ast)
        with _ -> ()
      in
      let ast_string = Stdlib.Buffer.contents buffer in
      let _ = Stdlib.Buffer.clear buffer in
      let parsed_ast =
        match Mtt.ParserInterface.parse_from_string Term ast_string with
        | Ok ast -> ast
        | Error _ -> Expr.Unit
      in
      Expr.equal ast parsed_ast)

let () = QCheck.Test.check_exn test
