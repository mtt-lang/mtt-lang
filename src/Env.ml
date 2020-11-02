open Base
open Result.Let_syntax

type 'a m = (Id.M.t * 'a) list
(** Modal environment *)

let emp_m = []

let extend_m delta id v = (id, v) :: delta

let lookup_m delta id =
  let typ_o = List.Assoc.find delta id ~equal:[%equal: Id.M.t] in
  match typ_o with
  | Some t -> return t
  | None ->
      let var_name = [%sexp_of: Id.M.t] id |> Sexp.to_string in
      Result.fail
        [%string "Variable $(var_name) is not found in the modal context!"]

type 'a r = (Id.R.t * 'a) list [@@deriving sexp]
(** Regular environment *)

let emp_r = []

let extend_r gamma id v = (id, v) :: gamma

let lookup_r gamma id =
  let typ_o = List.Assoc.find gamma id ~equal:[%equal: Id.R.t] in
  match typ_o with
  | Some t -> return t
  | None ->
      let var_name = [%sexp_of: Id.R.t] id |> Sexp.to_string in
      Result.fail
        [%string "Variable $(var_name) is not found in the regular context!"]
