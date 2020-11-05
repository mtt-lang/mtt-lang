open Mtt.Ast
open Mtt.Id
open Mtt.PrettyPrinter

(* QCheck generator for the arbitrary (and most likely invalid) expressions *)
let generator =
  QCheck.Gen.(
    sized
    @@ fix (fun self size ->
           let lowercase_id =
             string_size ~gen:(char_range 'a' 'z') (return 1)
           in
           match size with
           | 0 ->
               oneof
                 [
                   return Expr.Unit;
                   (* won't work until expressions with free variables can be parsed *)
                   (* map
                      (fun s -> Expr.VarL (R.mk s))
                      (string_size ~gen:(char_range 'a' 'z') (return 1)); *)
                   map (fun idg -> Expr.VarG (M.mk (idg ^ "'"))) lowercase_id;
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
                     (map (fun s -> R.mk s) lowercase_id)
                     (return Type.Unit)
                     (self (size - 1));
                   unary_node (fun x -> Expr.Box x);
                   map3
                     (fun x y z -> Expr.Letbox (x, y, z))
                     (map (fun s -> M.mk (s ^ "'")) lowercase_id)
                     (self (size / 2))
                     (self (size / 2));
                 ]))

(* Primitive expressions printer for testing purposes *)
let rec print_tree = function
  | Expr.Unit -> "Unit"
  | Expr.Pair (e1, e2) -> "Pair(" ^ print_tree e1 ^ ", " ^ print_tree e2 ^ ")"
  | Expr.Fst pe -> "Fst(" ^ print_tree pe ^ ")"
  | Expr.Snd pe -> "Snd(" ^ print_tree pe ^ ")"
  | Expr.VarL idl -> "VarL(" ^ R.to_string idl ^ ")"
  | Expr.VarG idg -> "VarG(" ^ M.to_string idg ^ ")"
  | Expr.App (fe, arge) -> "App(" ^ print_tree fe ^ "," ^ print_tree arge ^ ")"
  | Expr.Fun (idl, _, body) ->
      "Fun(" ^ R.to_string idl ^ ", Unit, " ^ print_tree body ^ ")"
  | Expr.Box e -> "Box(" ^ print_tree e ^ ")"
  | Expr.Letbox (idg, boxed_e, body) ->
      "App(" ^ M.to_string idg ^ "," ^ print_tree boxed_e ^ ", "
      ^ print_tree body ^ ")"

let arbitrary_tree = QCheck.make generator ~print:print_tree

let test =
  QCheck.Test.make ~name:"Expression pretty printer preserving syntax"
    ~count:100 arbitrary_tree (fun tree ->
      let buffer_size = 1024 in
      let buffer = Stdlib.Buffer.create buffer_size in
      let _ =
        try (PPrint.ToBuffer.pretty 1.0 80 buffer) (Doc.of_expr tree)
        with _ -> ()
      in
      let tree_string = Stdlib.Buffer.contents buffer in
      let parsed_tree =
        match Mtt.ParserInterface.parse_from_string Term tree_string with
        | Ok ast -> ast
        | Error _ -> Expr.Unit
      in
      tree = parsed_tree)

let () = QCheck.Test.check_exn test
