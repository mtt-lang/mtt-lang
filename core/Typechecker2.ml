open Ast.Expr

let translate_type ty =
  let open Ast.Type in
  match ty with
  | Unit -> "unit"
  | Base { idt } -> "(base \"" ^ idt ^ "\")"
  (* | Prod { ty1; ty2 } -> "pair" *)
  | _ -> failwith "TODO other types"

let rec translate Location.{ data = expr; _ } =
  match expr with
  | Unit -> "unit"
  | VarR { idr } -> Id.R.to_string idr
  | Fun { idr; ty_id; body } ->
      let ty_lower = translate_type ty_id in
      "fun (" ^ ty_lower ^ ") (" ^ Id.R.to_string idr ^ "\\" ^ translate body
      ^ ")"
  | App { fe; arge } ->
      "app (" ^ translate fe ^ ") " ^ "(" ^ translate arge ^ ")"
  | Pair { e1; e2 } -> "pair " ^ translate e1 ^ " " ^ translate e2
  | Fst { e } -> "fst " ^ translate e
  | Snd { e } -> "snd " ^ translate e
  | Let { idr; bound; body } ->
      "let (" ^ translate bound ^ ") " ^ "(" ^ Id.R.to_string idr ^ "\\"
      ^ translate body ^ ")"
  | _ -> failwith "not supported yet"
