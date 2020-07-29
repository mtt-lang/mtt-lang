open Base
open Result.Let_syntax

(** Global environment *)
type 'a g = (Id.G.t * 'a) list

let emp_g = []

let extend_g delta id v =
  (id, v) :: delta

let lookup_g delta id =
  let typ_o = List.Assoc.find delta id ~equal:[%equal: Id.G.t] in
  match typ_o with
  | Some t -> return t
  | None ->
      let var_name = [%sexp_of: Id.G.t] id |> Sexp.to_string in
      Result.fail [%string "Variable $(var_name) is not found in the global context!"]


(** Local environment *)
type 'a l = (Id.L.t * 'a) list [@@deriving sexp]

let emp_l = []

let extend_l gamma id v =
  (id, v) :: gamma

let lookup_l gamma id =
  let typ_o = List.Assoc.find gamma id ~equal:[%equal: Id.L.t] in
  match typ_o with
  | Some t -> return t
  | None ->
      let var_name = [%sexp_of: Id.L.t] id |> Sexp.to_string in
      Result.fail [%string "Variable $(var_name) is not found in the local context!"]
