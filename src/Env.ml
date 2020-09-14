open Base
open Result.Let_syntax

(** Modal environment *)
type 'a g = (Id.M.t * 'a) list

let emp_g = []

let extend_g delta id v =
  (id, v) :: delta

let lookup_g delta id =
  let typ_o = List.Assoc.find delta id ~equal:[%equal: Id.M.t] in
  match typ_o with
  | Some t -> return t
  | None ->
      let var_name = [%sexp_of: Id.M.t] id |> Sexp.to_string in
      Result.fail [%string "Variable $(var_name) is not found in the modal context!"]


(** Local environment *)
type 'a l = (Id.R.t * 'a) list [@@deriving sexp]

let emp_l = []

let extend_l gamma id v =
  (id, v) :: gamma

let lookup_l gamma id =
  let typ_o = List.Assoc.find gamma id ~equal:[%equal: Id.R.t] in
  match typ_o with
  | Some t -> return t
  | None ->
      let var_name = [%sexp_of: Id.R.t] id |> Sexp.to_string in
      Result.fail [%string "Variable $(var_name) is not found in the regular context!"]
