open Base
open Result.Let_syntax

type error = [ `EnvUnboundVariableError of string (* name of variable *)
                                        * string (* message *)
             ]

module Make (Key : sig
  type t [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_sexp_conv_lib.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  include Equal.S with type t := t

  val context_kind : string
end) =
struct
  type 'v t = (Key.t, 'v) List.Assoc.t [@@deriving sexp]
  (** Environment *)

  (** Empty environment *)
  let emp = []

  (** Extend environment with a key and the corresponding value *)
  let extend env k v = (k, v) :: env

  (** Find the value corresponding to a key identifier *)
  let lookup env k =
    let typ_o = List.Assoc.find env k ~equal:Key.equal in
    match typ_o with
    | Some t -> return t
    | None ->
        let var_name = [%sexp_of: Key.t] k |> Sexp.to_string in
        Result.fail @@ `EnvUnboundVariableError (var_name,
          [%string "$(var_name) is not found in the $(Key.context_kind) environment!"])
end

module R = Make (Id.R)
(** Regular environment *)

module M = Make (Id.M)
(** Modal environment *)
